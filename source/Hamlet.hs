module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
  hamletText <- Text.IO.readFile "hamlet.txt"
  let
    hamletLines = Text.lines hamletText
  print $ length hamletLines
