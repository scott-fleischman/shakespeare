{-# LANGUAGE OverloadedStrings #-}

module Shakespeare.Hamlet where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import           Formatting ((%))
import qualified Formatting
import           Prelude hiding (lines)

main :: IO ()
main = do
  hamletText <- Text.IO.readFile "hamlet.txt"
  let lines = getTextLines hamletText
  trails <- eitherToError . getTrails $ lines
  let
    trailsWithTails = filter ((> 1) . trailBlankLines) trails
    formatLine (Line (LineNumber n) t) = Formatting.format (Formatting.right 5 ' ' % Formatting.stext) n t
    formatTrail (Trail l c) = Formatting.format (Formatting.right 5 ' ' % Formatting.text) c (formatLine l)
  mapM_ (Text.Lazy.IO.putStrLn . formatTrail) trailsWithTails

  outline <- eitherToError $ parseOutline trails
  Text.Lazy.IO.putStrLn $ Formatting.format ("title: " % Formatting.text) (formatTrail $ outlineTitle outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("author: " % Formatting.text) (formatTrail $ outlineAuthor outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("contents count: " % Formatting.shown) (length $outlineContents outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("actors count: " % Formatting.shown) (length $outlineActors outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("acts count: " % Formatting.shown) (length $ outlineActs outline)
  print $ renderOutline outline == hamletText

eitherToError :: Show a => Either a b -> IO b
eitherToError (Left err) = fail . show $ err
eitherToError (Right x) = return x

data Outline = Outline
  { outlineTitle :: Trail
  , outlineAuthor :: Trail
  , outlineContents :: [Trail]
  , outlineActors :: [Trail]
  , outlineActs :: [[Trail]]
  } deriving Show

data OutlineError
  = OutlineNoTitle
  | OutlineNoAuthor
  deriving Show

parseOutline :: [Trail] -> Either OutlineError Outline
parseOutline input = do
  (title, afterTitle) <-
    case input of
      [] -> Left OutlineNoTitle
      x : xs -> Right (x, xs)
  (author, afterAuthor) <-
    case afterTitle of
      [] -> Left OutlineNoAuthor
      x : xs -> Right (x, xs)
  let (contents, afterContents) = takeWhileExtraSplit ((<= 1) . trailBlankLines) afterAuthor
  let (actors, afterActors) = takeWhileExtraSplit ((<= 1) . trailBlankLines) afterContents
  let acts = parseActs afterActors
  Right $ Outline title author contents actors acts

renderOutline :: Outline -> Text
renderOutline (Outline title author contents actors acts) =
  mapAppendEndline $
    [ renderTrail title
    , renderTrail author
    , renderTrails contents
    , renderTrails actors
    ]
    ++ fmap renderTrails acts

mapAppendEndline :: [Text] -> Text
mapAppendEndline = Text.concat . fmap (flip Text.append "\n")

addFinalEndline :: Text -> Text
addFinalEndline input = Text.append input "\n"

renderTrails :: [Trail] -> Text
renderTrails = Text.intercalate "\n" . fmap renderTrail

renderTrail :: Trail -> Text
renderTrail (Trail (Line _ text) blanks) = Text.concat $ text : replicate blanks "\n"

parseActs :: [Trail] -> [[Trail]]
parseActs [] = []
parseActs input@(_ : _) =
  let (current, after) = takeWhileExtraSplit ((<= 1) . trailBlankLines) input
  in case after of
    [] -> [current]
    xs@(_ : _) -> let results = parseActs xs in current : results

takeWhileExtraSplit :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileExtraSplit _ [] = ([], [])
takeWhileExtraSplit f (x : xs) =
  if f x
    then let (r1, r2) = takeWhileExtraSplit f xs in (x : r1, r2)
    else ([x], xs)

data Line = Line
  { lineNumber :: LineNumber
  , lineText :: Text
  } deriving Show
newtype LineNumber = LineNumber Int deriving Show

getTextLines :: Text -> [Line]
getTextLines = zipWith Line lineNumbers . Text.lines
  where
  lineNumbers = fmap LineNumber [1..]

data Trail = Trail
  { trailLine :: Line
  , trailBlankLines :: Int
  } deriving Show

data InitialBlankLine = InitialBlankLine deriving Show

isBlankLine :: Line -> Bool
isBlankLine = Text.null . lineText

getTrails :: [Line] -> Either InitialBlankLine [Trail]
getTrails [] = Right []
getTrails (x : _) | isBlankLine x = Left InitialBlankLine
getTrails (x : xs) = Right $ getTrailsRec x 0 xs

getTrailsRec :: Line -> Int -> [Line] -> [Trail]
getTrailsRec x c [] = [Trail x c]
getTrailsRec x c (y : ys) | isBlankLine y = getTrailsRec x (c + 1) ys
getTrailsRec x c (y : ys) = Trail x c : getTrailsRec y 0 ys
