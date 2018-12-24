{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import qualified Data.ByteString.Zipper        as Z
import qualified Data.ByteString.UTF8          as BSU8
import qualified Data.ByteString.Unsafe        as BSU

import qualified Widgets.Edit                  as ED

import           TreeSitter.CursorApi.Cursor
import           TreeSitter.Haskell

import           Foreign.ForeignPtr
import           Data.Maybe

import qualified Data.Semigroup                as Sem
import           Data.String                    ( IsString(..) )

import qualified Graphics.Vty                  as V
import           Brick.Util                     ( fg
                                                , bg
                                                )

import           Text.Pretty.Simple             ( pPrint )
import           Control.Exception.Assert


main :: IO ()
main =
  let source = "odule A where"
      z      = Z.byteStringZipper (BSU8.lines $ BSU8.fromString source) Nothing
      z'     = Z.insertChar 'm' z
  in  do
        let bs = BSU8.unlines $ Z.getByteString z'

        (newTree, newMarkup) <-
          BSU.unsafeUseAsCStringLen bs $ \(str, len) -> do
            fgnPtrCursor <- mallocForeignPtr :: IO (ForeignPtr Cursor)
            withForeignPtr fgnPtrCursor $ \cur -> do
              tree <- hts_parse_with_language tree_sitter_haskell
                                              str
                                              (fromIntegral len)
              -- tree <- hts_parser_parse_string str (fromIntegral len)
              ts_cursor_reset_root tree cur
              (pos, (_, markup)) <- tsTransformMarkup bs cur
              return (tree, markup)

        pPrint newMarkup
        let Markup (bs, as) = newMarkup
        print $ byEq assert "lengths of Markup components differ" (length bs) (length as) "Markup lengths Ok"


-- from brick Markup

newtype Markup a = Markup ([BSU8.ByteString], [a]) deriving Show

instance Sem.Semigroup (Markup a) where
    (Markup (bs1, a1)) <> (Markup (bs2, a2)) = Markup (bs1 <> bs2, a1 <> a2)

instance Monoid (Markup a) where
    mempty = Markup mempty
    mappend = (Sem.<>)

fromByteString :: (Monoid a) => BSU8.ByteString -> Markup a
fromByteString = (@@ mempty)

(@@) :: BSU8.ByteString -> a -> Markup a
bs @@ a = Markup ([bs], [a])


-- from Edit.hs

tsTransformMarkup
  :: BSU8.ByteString -> PtrCursor -> IO (Int, (BSU8.ByteString, Markup V.Attr))
tsTransformMarkup bs = ED.tsTransform (curopsMarkup bs)

curopsMarkup
  :: BSU8.ByteString
  -> CursorOperations PtrCursor IO (Int, (BSU8.ByteString, Markup V.Attr))
curopsMarkup bs = CursorOperations
  { initResult     = initMarkup bs 0
  , packNode       = packNodeMarkup
  , nodeFirstChild = firstChild
  , nodeNext       = next
  , nodeParent     = parent
  }

initMarkup
  :: BSU8.ByteString
  -> Int
  -> PtrCursor
  -> IO (Int, (BSU8.ByteString, Markup V.Attr))
initMarkup bs pos ptrCur = do
  (pos', mup) <- spanInfoAdvance bs pos ptrCur mempty
  return (pos', (bs, mup))

packNodeMarkup
  :: PtrCursor
  -> Navigation
  -> (Int, (BSU8.ByteString, Markup V.Attr))
  -> IO (Int, (BSU8.ByteString, Markup V.Attr))
packNodeMarkup ptrCur nav (pos, (bs, mup)) = case nav of
  TreeSitter.CursorApi.Cursor.Down -> do
    (pos', mup') <- spanInfoAdvance bs pos ptrCur mup
    return (pos', (bs, mup'))

  TreeSitter.CursorApi.Cursor.Next -> do
    (pos', mup') <- spanInfoAdvance bs pos ptrCur mup
    return (pos', (bs, mup'))

  TreeSitter.CursorApi.Cursor.Up -> do
    p <- hasParent
    case p of
      True -> return (pos, (bs, mup))
      False ->
        let markupTail = if pos < BSU8.length bs
              then fromByteString $ BSU8.drop pos bs
              else mempty
        in  return (pos, (bs, mup <> markupTail))

spanInfoAdvance
  :: BSU8.ByteString
  -> Int
  -> PtrCursor
  -> Markup V.Attr
  -> IO (Int, Markup V.Attr)
spanInfoAdvance bs pos ptrCur mup = do
  spanInfo <- spanInfoFromCursor ptrCur
  case spanInfo of
    Token _ _ _ -> advanceToken spanInfo
    _           -> return (pos, mup)
        -- TODO produce Error markdown-style
        -- Error start end    -> return (pos, mup)
 where
  advanceToken (Token start end _) =
    let (start', end') =
          if start == 1 && end == 1 then (0, 1) else (start, end) -- TODO
        bs'        = BSU8.drop pos bs
        d          = start' - pos
        dn         = end' - start'

        bsBefore   = BSU8.take d bs'
        markupBS   = bsBefore @@ bg V.blue

        nodeBS     = BSU8.take dn (BSU8.drop d bs')
        markupNode = nodeBS @@ fg V.blue

        mup'       = mup <> markupBS <> markupNode
    in  return (end', mup')

