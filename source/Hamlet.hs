{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
  hamletText <- Text.IO.readFile "hamlet.txt"
  let
    hamletLines = Text.lines hamletText
    isNameLetterSpace x = Char.isUpper x || Char.isSpace x
    isNamePrefix x
      = Text.takeEnd 1 x == "."
      && (Text.all isNameLetterSpace . Text.dropEnd 1) x
    hamletNames = (Set.fromList . filter isNamePrefix) hamletLines
  mapM_ Text.IO.putStrLn hamletNames
