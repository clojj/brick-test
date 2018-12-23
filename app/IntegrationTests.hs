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


main :: IO ()
main =
  let source = "odule A where"
      z      = Z.byteStringZipper (BSU8.lines $ BSU8.fromString source) Nothing
      z'     = Z.insertChar 'm' $ Z.moveRight z
  in  do
        let text = BSU8.unlines $ Z.getText z'

        (newTree, newMarkup) <-
          BSU.unsafeUseAsCStringLen text $ \(str, len) -> do
            fgnPtrCursor <- mallocForeignPtr :: IO (ForeignPtr Cursor)
            withForeignPtr fgnPtrCursor $ \cur -> do
              tree <- hts_parse_with_language tree_sitter_haskell
                                              str
                                              (fromIntegral len)
              tree <- hts_parser_parse_string str (fromIntegral len)
              ts_cursor_reset_root tree cur
              (pos, (_, markup)) <- ED.tsTransformMarkup text cur
              return (tree, markup)

        -- print newMarkup
        print "STOP"
