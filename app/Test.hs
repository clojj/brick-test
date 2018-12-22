{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import qualified Data.ByteString.Zipper as Z
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main ∷ IO ()
main = 
  
  let s    = "12ε3"
      z    = Z.moveRight $ Z.byteStringZipper (BS.lines $ BS.fromString s) Nothing

      z'   = Z.insertMany' "ε" z
      z''  = Z.deletePrevChar $ Z.moveLeft z'
      txt  = TE.decodeUtf8 (BS.unlines (Z.getText z'))
      txt' = TE.decodeUtf8 (BS.unlines (Z.getText z''))

    in do
      putStrLn $ T.unpack txt
      putStrLn $ T.unpack txt'
