{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
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
import qualified Data.ByteString.Zipper as Z
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Monad.IO.Class (liftIO)
import System.IO

import Brick.Types
import Brick.Widgets.Core
import Brick.Util (on, fg, bg)
import Brick.Markup (markup, Markup(..))
import Brick.AttrMap (attrMap, AttrMap, AttrName)
import Data.Text.Markup ((@@), fromText, markupToList, isEmpty)

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
    Editor { editContents :: Z.ByteStringZipper t
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


tsTransformMarkup :: BS.ByteString -> PtrCursor -> IO (Int, (BS.ByteString, Markup V.Attr))
tsTransformMarkup text = tsTransform (curopsWidget text)

curopsWidget :: BS.ByteString -> CursorOperations PtrCursor IO (Int, (BS.ByteString, Markup V.Attr))
curopsWidget text = CursorOperations
            { initResult     = initWidget text 0
            , packNode       = packNodeWidget
            , nodeFirstChild = firstChild
            , nodeNext       = next
            , nodeParent     = parent
            }

initWidget :: BS.ByteString -> Int -> PtrCursor -> IO (Int, (BS.ByteString, Markup V.Attr))
initWidget text pos ptrCur = do
    (pos', mup) <- spanInfoAdvance text pos ptrCur mempty
    return (pos', (text, mup))

packNodeWidget :: PtrCursor -> Navigation -> (Int, (BS.ByteString, Markup V.Attr)) -> IO (Int, (BS.ByteString, Markup V.Attr))
packNodeWidget ptrCur nav (pos, (text, mup)) = 
  case nav of
      TreeSitter.CursorApi.Cursor.Down -> do
        (pos', mup') <- spanInfoAdvance text pos ptrCur mup
        return (pos', (text, mup'))

      TreeSitter.CursorApi.Cursor.Next -> do
        (pos', mup') <- spanInfoAdvance text pos ptrCur mup
        return (pos', (text, mup'))

      TreeSitter.CursorApi.Cursor.Up -> do
        p <- hasParent
        case p of
            True  -> return (pos, (text, mup))
            False -> let markupTail = if pos < BS.length text then fromText $ TE.decodeUtf8 $ BS.drop pos text else mempty
                     in return (pos, (text, mup <> markupTail))

spanInfoAdvance :: BS.ByteString -> Int -> PtrCursor -> Markup V.Attr -> IO (Int, Markup V.Attr)
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
                text'         = BS.drop pos text
                d             = start' - pos
                dn            = end' - start'

                textBefore    = TE.decodeUtf8 $ BS.take d text'
                markupText    = textBefore @@ bg V.green

                nodeTxt       = TE.decodeUtf8 $ BS.take dn (BS.drop d text')
                markupNode    = nodeTxt @@ fg V.blue

                mup'          = mup <> markupText <> markupNode
            in return (end', mup')


initEvent :: Editor BS.ByteString n -> EventM n (Editor BS.ByteString n)
initEvent ed = do
  (fpc, initialTree) <- liftIO $ 
    BSU.unsafeUseAsCStringLen (BS.unlines $ Z.getText (editContents ed)) $ \ (str, len) -> do
        tree       <- hts_parse_with_language tree_sitter_haskell str (fromIntegral len)
        fgnPtrCursor <- mallocForeignPtr :: IO (ForeignPtr Cursor)
        -- addForeignPtrFinalizer funptr_ts_cursor_free fgnPtrCursor
        withForeignPtr fgnPtrCursor $ \cur -> ts_cursor_init tree cur
        return (fgnPtrCursor, tree)
  return $ ed { tree = Just initialTree, fgnPtrCursor = Just fpc }

type ZipperFunction a = Z.ByteStringZipper a -> Z.ByteStringZipper a

data EditEvent a = Nav (ZipperFunction a) | Mod (ZipperFunction a)

getEvent :: EditEvent a -> ZipperFunction a
getEvent ee = case ee of
    Nav e -> e
    Mod e -> e

handleEditorEvent :: Event -> Editor BS.ByteString n -> EventM n (Editor BS.ByteString n)
handleEditorEvent e ed =
    let f = case e of
                EvKey (KChar 'q') [MCtrl]       -> Mod (Z.insertMany (BS.fromString "module many--ε")) -- TODO remove (just testing)

                EvKey (KChar 'a') [MCtrl]       -> Nav Z.gotoBOL
                EvKey (KChar 'e') [MCtrl]       -> Nav Z.gotoEOL
                EvKey KUp []                    -> Nav Z.moveUp
                EvKey KDown []                  -> Nav Z.moveDown
                EvKey KLeft []                  -> Nav Z.moveLeft
                EvKey KRight []                 -> Nav Z.moveRight

                EvKey (KChar 'd') [MCtrl]       -> Mod Z.deleteChar
                EvKey (KChar 'k') [MCtrl]       -> Mod Z.killToEOL
                EvKey (KChar 'u') [MCtrl]       -> Mod Z.killToBOL
                EvKey KEnter []                 -> Mod Z.breakLine
                EvKey KDel []                   -> Mod Z.deleteChar
                EvKey (KChar c) [] | c /= '\t'  -> Mod (Z.insertChar c)
                EvKey KBS []                    -> Mod Z.deletePrevChar

                _                               -> Nav id
    in do
        let ed' = applyEdit (getEvent f) ed
        case f of
            Nav _ -> return ed'
            Mod _ -> do
                        (newTree, newMarkup) <- liftIO $ do
                            let text = BS.unlines $ Z.getText (editContents ed')

                            BSU.unsafeUseAsCStringLen text $ \ (str, len) -> 

                                withForeignPtr (fromJust $ fgnPtrCursor ed') $ \cur -> do
                                    tree               <- hts_parser_parse_string str (fromIntegral len)
                                    ts_cursor_reset_root tree cur
                                    hPrint stderr "--- start tree ------------------"
                                    (pos, (_, markup)) <- tsTransformMarkup text cur
                                    hPrint stderr "--- end tree ------------------"
                                    return (tree, markup)

                        -- liftIO $ hPrint stderr $ markupToList newMarkup
                        return ed' { tree = Just newTree, mup = Just newMarkup }

-- | Construct an editor over 'Text' values
editorText :: n
       -- ^ The editor's name (must be unique)
       -> Maybe Int
       -- ^ The limit on the number of lines in the editor ('Nothing'
       -- means no limit)
       -> BS.ByteString
       -- ^ The initial content
       -> Editor BS.ByteString n
editorText name limit bs = Editor (Z.byteStringZipper (BS.lines bs) limit) name Nothing Nothing (Just $ fromText (TE.decodeUtf8 bs))

-- | Apply an editing operation to the editor's contents. Bear in mind
-- that you should only apply zipper operations that operate on the
-- current line; the editor will only ever render the first line of
-- text.
applyEdit :: (Z.ByteStringZipper t -> Z.ByteStringZipper t)
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
             => ([BS.ByteString] -> Widget n)
             -- ^ The content drawing function
             -> Bool
             -- ^ Whether the editor has focus. It will report a cursor
             -- position if and only if it has focus.
             -> Editor BS.ByteString n
             -- ^ The editor.
             -> Widget n
renderEditor draw foc e = -- TODO remove draw
    let cp = Z.cursorPosition z
        z = e^.editContentsL
        toLeft = TE.decodeUtf8 $ BS.take (cp^._2) (Z.currentLine z)
        cursorLoc = Location (textWidth toLeft, cp^._1)
        limit = case e^.editContentsL.to Z.getLineLimit of
            Nothing -> id
            Just lim -> vLimit lim
        atChar = charAtCursor $ e^.editContentsL
        atCharWidth = maybe 1 (textWidth . TE.decodeUtf8) atChar
    in withAttr (if foc then editFocusedAttr else editAttr) $
       limit $
       viewport (e^.editorNameL) Both $
       (if foc then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
       case mup e of
            Nothing -> txt ""
            Just m ->
                if isEmpty m
                    then txt $ TE.decodeUtf8 $ BS.unlines $ getEditContents e
                    else markup m
       

charAtCursor :: Z.ByteStringZipper BS.ByteString -> Maybe BS.ByteString
charAtCursor z =
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        toRight = BS.drop col curLine
    in if BS.length toRight > 0
       then Just $ BS.take 1 toRight
       else Nothing
