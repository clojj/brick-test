{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- | This module provides a basic text editor widget. You'll need to
-- embed an 'Editor' in your application state and transform it with
-- 'handleEditorEvent' when relevant events arrive. To get the contents
-- of the editor, just use 'getEditContents'. To modify it, use the
-- 'Z.TextZipper' interface with 'applyEdit'.
--
-- The editor's 'handleEditorEvent' function handles a set of basic
-- input events that should suffice for most purposes; see the source
-- for a complete list.
--
-- Bear in mind that the editor provided by this module is intended to
-- provide basic input support for brick applications but it is not
-- intended to be a replacement for your favorite editor such as Vim or
-- Emacs. It is also not suitable for building sophisticated editors. If
-- you want to build your own editor, I suggest starting from scratch.
module Widgets.Edit
  ( Editor(editContents, editorName)
  , initEvent
  -- * Constructing an editor
  , editor
  , editorText
  -- * Reading editor contents
  , getEditContents
  -- * Handling events
  , handleEditorEvent
  -- * Editing text
  , applyEdit
  -- * Lenses for working with editors
  , editContentsL
  -- * Rendering editors
  , renderEditor
  -- * Attributes
  , editAttr
  , editFocusedAttr
  )
where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Lens.Micro
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import qualified Graphics.Vty as V

import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as Z

import Control.Monad.IO.Class (liftIO)
import System.IO

import Brick.Types
import Brick.Widgets.Core
import Brick.Util (on, fg, bg)
import Brick.Markup (markup, (@?), Markup(..))
import Brick.AttrMap (attrMap, AttrMap, AttrName)
import Data.Text.Markup ((@@), fromText, markupToList)

import           TreeSitter.CursorApi.Cursor
import           TreeSitter.CursorApi.Types

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.Haskell
import           TreeSitter.Node
import           TreeSitter.TsInputEdit
import           TreeSitter.TsPoint

import           Foreign.ForeignPtr
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                )
import Data.Maybe

-- | Editor state.  Editors support the following events by default:
--
-- * Ctrl-a: go to beginning of line
-- * Ctrl-e: go to end of line
-- * Ctrl-d, Del: delete character at cursor position
-- * Backspace: delete character prior to cursor position
-- * Ctrl-k: delete all from cursor to end of line
-- * Ctrl-u: delete all from cursor to beginning of line
-- * Arrow keys: move cursor
-- * Enter: break the current line at the cursor position
data Editor t n =
    Editor { editContents :: Z.TextZipper t
           -- ^ The contents of the editor
           , editorName :: n
           -- ^ The name of the editor

           -- TODO get rid these Maybe's
           , tree :: Maybe (Ptr Tree)
           , fgnPtrCursor :: Maybe (ForeignPtr Cursor)
           , mup :: Maybe (Markup V.Attr)
           }

suffixLenses ''Editor

instance (Show t, Show n) => Show (Editor t n) where
    show e =
        concat [ "Editor { "
               , "editContents = " <> show (editContents e)
               , ", editorName = " <> show (editorName e)
               , "}"
               ]

instance Named (Editor t n) n where
    getName = editorName


tsTransformMarkup :: T.Text -> PtrCursor -> IO (Int, (T.Text, Markup V.Attr))
tsTransformMarkup text = tsTransform (curopsWidget text)

curopsWidget :: T.Text -> CursorOperations PtrCursor IO (Int, (T.Text, Markup V.Attr))
curopsWidget text = CursorOperations
            { initResult     = initWidget text 0
            , packNode       = packNodeWidget
            , nodeFirstChild = firstChild
            , nodeNext       = next
            , nodeParent     = parent
            }

initWidget :: T.Text -> Int -> PtrCursor -> IO (Int, (T.Text, Markup V.Attr))
initWidget text pos ptrCur = do
    (pos', mup) <- spanInfoAdvance text pos ptrCur mempty
    return (pos', (text, mup))

packNodeWidget :: PtrCursor -> Navigation -> (Int, (T.Text, Markup V.Attr)) -> IO (Int, (T.Text, Markup V.Attr))
packNodeWidget ptrCur nav (pos, (text, mup)) = 
  case nav of
      TreeSitter.CursorApi.Cursor.Down -> do
        (pos', mup') <- spanInfoAdvance text pos ptrCur mup
        return (pos', (text, mup'))

      TreeSitter.CursorApi.Cursor.Next -> do
        (pos', mup') <- spanInfoAdvance text pos ptrCur mup
        return (pos', (text, mup'))

      TreeSitter.CursorApi.Cursor.Up -> return (pos, (text, mup))

spanInfoAdvance :: T.Text -> Int -> PtrCursor -> Markup V.Attr -> IO (Int, Markup V.Attr)
spanInfoAdvance text pos ptrCur mup = do
  spanInfo <- spanInfoFromCursor ptrCur
--   hPrint stderr pos
  hPrint stderr spanInfo
  case spanInfo of
        Token _ _ _  -> advanceToken spanInfo
        _            -> return (pos, mup)
        -- TODO produce Error markdown-style
        -- Error start end    -> return (pos, mup)

  where advanceToken (Token start end _) = 
            let (start', end') = if start == 1 && end == 1 then (0, 1) else (start, end) -- TODO
                text'         = T.drop pos text
                d             = start' - pos
                dn            = end' - start'

                textBefore    = T.take d text'
                markupText    = textBefore @@ bg V.green

                nodeTxt       = T.take dn (T.drop d text')
                markupNode    = nodeTxt @@ fg V.blue

                mup'          = mup <> markupText <> markupNode
            in return (end', mup')


initEvent :: Editor T.Text n -> EventM n (Editor T.Text n)
initEvent ed = do
  (fpc, initialTree) <- liftIO $ 
    BSU.unsafeUseAsCStringLen (DTE.encodeUtf16LE (T.unlines $ Z.getText (editContents ed))) $ \ (str, len) -> do
        -- (str, len) <- newCStringLen $ T.unpack $ T.unlines $ Z.getText (editContents ed)
        tree       <- hts_parse_with_language tree_sitter_haskell str (fromIntegral len)
        fgnPtrCursor <- mallocForeignPtr :: IO (ForeignPtr Cursor)
        -- addForeignPtrFinalizer funptr_ts_cursor_free fgnPtrCursor
        withForeignPtr fgnPtrCursor $ \cur -> ts_cursor_init tree cur
        return (fgnPtrCursor, tree)
  return $ ed { tree = Just initialTree, fgnPtrCursor = Just fpc }

handleEditorEvent :: Event -> Editor T.Text n -> EventM n (Editor T.Text n)
handleEditorEvent e ed =
    let f = case e of
                EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
                EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
                EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
                EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
                EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
                EvKey KEnter [] -> Z.breakLine
                EvKey KDel [] -> Z.deleteChar
                EvKey (KChar c) [] | c /= '\t' -> Z.insertChar c
                EvKey KUp [] -> Z.moveUp
                EvKey KDown [] -> Z.moveDown
                EvKey KLeft [] -> Z.moveLeft
                EvKey KRight [] -> Z.moveRight
                EvKey KBS [] -> Z.deletePrevChar
                _ -> id
    in do
        let ed' = applyEdit f ed
        (newTree, newMarkup) <- liftIO $ do
            let text = T.unlines $ Z.getText (editContents ed')

            -- TODO
            BSU.unsafeUseAsCStringLen (DTE.encodeUtf16LE text) $ \ (str, len) -> 

            -- TODO
            -- let acquire = ts_parser_parse_string parser nullPtr bytes len
                -- release t
                --     | t == nullPtr = pure ()
                --     | otherwise = ts_tree_delete t
            -- bracket acquire release (\ treePtr -> …)

            -- (str, len) <- newCStringLen $ T.unpack text -- TODO remove ?

                withForeignPtr (fromJust $ fgnPtrCursor ed') $ \cur -> do
                    tree               <- hts_parser_parse_string str (fromIntegral len)
                    ts_cursor_reset_root tree cur
                    hPrint stderr "--- start tree ------------------"
                    (pos, (_, markup)) <- tsTransformMarkup text cur
                    hPrint stderr "--- end tree ------------------"
                    let markupTail = if pos < T.length text then fromText (T.drop pos text) else mempty
                    return (tree, markup <> markupTail)

        -- liftIO $ hPrint stderr $ markupToList newMarkup
        return ed' { tree = Just newTree, mup = Just newMarkup }

-- | Construct an editor over 'Text' values
editorText :: n
       -- ^ The editor's name (must be unique)
       -> Maybe Int
       -- ^ The limit on the number of lines in the editor ('Nothing'
       -- means no limit)
       -> T.Text
       -- ^ The initial content
       -> Maybe (Ptr Tree)
       -> Maybe (ForeignPtr Cursor)
       -> Maybe (Markup V.Attr)
       -> Editor T.Text n
editorText = editor

-- | Construct an editor over 'String' values
editor :: Z.GenericTextZipper a
       => n
       -- ^ The editor's name (must be unique)
       -> Maybe Int
       -- ^ The limit on the number of lines in the editor ('Nothing'
       -- means no limit)
       -> a
       -- ^ The initial content
       -> Maybe (Ptr Tree)
       -> Maybe (ForeignPtr Cursor)
       -> Maybe (Markup V.Attr)
       -> Editor a n
editor name limit s = Editor (Z.textZipper (Z.lines s) limit) name

-- | Apply an editing operation to the editor's contents. Bear in mind
-- that you should only apply zipper operations that operate on the
-- current line; the editor will only ever render the first line of
-- text.
applyEdit :: (Z.TextZipper t -> Z.TextZipper t)
          -- ^ The 'Data.Text.Zipper' editing transformation to apply
          -> Editor t n
          -> Editor t n
applyEdit f e = e & editContentsL %~ f

-- | The attribute assigned to the editor when it does not have focus.
editAttr :: AttrName
editAttr = "edit"

-- | The attribute assigned to the editor when it has focus. Extends
-- 'editAttr'.
editFocusedAttr :: AttrName
editFocusedAttr = editAttr <> "focused"

-- | Get the contents of the editor.
getEditContents :: Monoid t => Editor t n -> [t]
getEditContents e = Z.getText $ e^.editContentsL

-- | Turn an editor state value into a widget. This uses the editor's
-- name for its scrollable viewport handle and the name is also used to
-- report mouse events.
renderEditor :: (Ord n, Show n)
             => ([T.Text] -> Widget n)
             -- ^ The content drawing function
             -> Bool
             -- ^ Whether the editor has focus. It will report a cursor
             -- position if and only if it has focus.
             -> Editor T.Text n
             -- ^ The editor.
             -> Widget n
renderEditor draw foc e = -- TODO remove draw
    let cp = Z.cursorPosition z
        z = e^.editContentsL
        toLeft = Z.take (cp^._2) (Z.currentLine z)
        cursorLoc = Location (textWidth toLeft, cp^._1)
        limit = case e^.editContentsL.to Z.getLineLimit of
            Nothing -> id
            Just lim -> vLimit lim
        atChar = charAtCursor $ e^.editContentsL
        atCharWidth = maybe 1 textWidth atChar
    in withAttr (if foc then editFocusedAttr else editAttr) $
       limit $
       viewport (e^.editorNameL) Both $
       (if foc then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
    --    txt $ T.unlines $ Z.getText z
       case mup e of
            Nothing -> txt ""
            -- Just ws -> hBox ws
            Just m -> 
                case show m of -- TODO
                    "Markup []" -> txt $ T.unlines $ getEditContents e 
                    _ -> markup m
       

charAtCursor :: (Z.GenericTextZipper t) => Z.TextZipper t -> Maybe t
charAtCursor z =
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        toRight = Z.drop col curLine
    in if Z.length toRight > 0
       then Just $ Z.take 1 toRight
       else Nothing
