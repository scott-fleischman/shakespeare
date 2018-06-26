{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Shakespeare.Hamlet where

import qualified Control.Lens as Lens
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
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

  outline <- eitherToError $ parseOutline2 trails
  Text.Lazy.IO.putStrLn $ Formatting.format ("title: " % Formatting.stext) ((\(Title t) -> t) $ outlineTitle outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("author: " % Formatting.stext) ((\(Author t) -> t) $ outlineAuthor outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("contents count: " % Formatting.shown) (length $outlineContents outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("actors count: " % Formatting.shown) (length $outlineActors outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("acts count: " % Formatting.shown) (length $ outlineActs outline)

  mapM_ (Text.Lazy.IO.putStrLn . formatTrail) (filter ((> 0) . trailBlankLines) $ outlineContents outline)

formatLine :: Line -> Text.Lazy.Text
formatLine (Line (LineNumber n) t) = Formatting.format (Formatting.right 5 ' ' % Formatting.stext) n t

formatTrail :: Trail -> Text.Lazy.Text
formatTrail (Trail l c) = Formatting.format (Formatting.right 5 ' ' % Formatting.text) c (formatLine l)

eitherToError :: Show a => Either a b -> IO b
eitherToError (Left err) = fail . show $ err
eitherToError (Right x) = return x

data OutlineV a b c d e = Outline
  { outlineTitle :: a
  , outlineAuthor :: b
  , outlineContents :: c
  , outlineActors :: d
  , outlineActs :: e
  } deriving Show

outlineTitleLens :: forall a a' b c d e f. Functor f => (a -> f a') -> OutlineV a b c d e -> f (OutlineV a' b c d e)
outlineTitleLens f (Outline a b c d e) = fmap (\a' -> Outline a' b c d e) (f a)

outlineAuthorLens :: forall a b b' c d e f. Functor f => (b -> f b') -> OutlineV a b c d e -> f (OutlineV a b' c d e)
outlineAuthorLens f (Outline a b c d e) = fmap (\b' -> Outline a b' c d e) (f b)

type Outline1 = OutlineV Trail Trail [Trail] [Trail] [[Trail]]
type Outline2 = OutlineV Title Author [Trail] [Trail] [[Trail]]

data OutlineTrailError
  = OutlineNoTitle
  | OutlineNoAuthor
  deriving Show

newtype Title = Title Text
newtype Author = Author Text

data TitleError = TitleInvalidBlankLines Int deriving Show
data AuthorError = AuthorInvalidBlankLines Int deriving Show

data AllError
  = AllErrorOutlineTrail OutlineTrailError
  | AllErrorTitle TitleError
  | AllErrorAuthor AuthorError
  deriving Show

parseOutline2 :: [Trail] -> Either AllError Outline2
parseOutline2 input = do
  step1 <- Lens.over Lens._Left AllErrorOutlineTrail $
    parseOutline1 input
  step2 <- Lens.over Lens._Left AllErrorTitle $
    outlineTitleLens parseTitle step1
  step3 <- Lens.over Lens._Left AllErrorAuthor $
    outlineAuthorLens parseAuthor step2
  return step3

parseAuthor :: Trail -> Either AuthorError Author
parseAuthor (Trail (Line _ t) c) =
  if c == authorBlankLineCount
    then Right $ Author t
    else Left $ AuthorInvalidBlankLines c

authorBlankLineCount :: Int
authorBlankLineCount = 6

renderAuthor :: Author -> Text
renderAuthor (Author t) = Text.concat $ t : makeBlankLines authorBlankLineCount

parseTitle :: Trail -> Either TitleError Title
parseTitle (Trail (Line _ t) c) =
  if c == titleBlankLineCount
    then Right $ Title t
    else Left $ TitleInvalidBlankLines c

titleBlankLineCount :: Int
titleBlankLineCount = 3

renderTitle :: Title -> Text
renderTitle (Title t) = Text.concat $ t : makeBlankLines titleBlankLineCount

parseOutline1 :: [Trail] -> Either OutlineTrailError Outline1
parseOutline1 input = do
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
  let acts = parseActsTrail afterActors
  Right $ Outline title author contents actors acts

renderOutlineV
  :: OutlineV (a -> Text) (b -> Text) (c -> Text) (d -> Text) (e -> Text)
  -> OutlineV a b c d e
  -> Text
renderOutlineV (Outline fa fb fc fd fe) (Outline a b c d e) =
  mapAppendEndline $
    [ fa a
    , fb b
    , fc c
    , fd d
    , fe e
    ]

outline1Renderer :: OutlineV
  (Trail      -> Text)
  (Trail      -> Text)
  ([Trail]    -> Text)
  ([Trail]    -> Text)
  ([[Trail]]  -> Text)
outline1Renderer =
  Outline
    renderTrail
    renderTrail
    renderTrails
    renderTrails
    (Text.intercalate "\n" . fmap renderTrails)

outline2Renderer :: OutlineV
  (Title      -> Text)
  (Author     -> Text)
  ([Trail]    -> Text)
  ([Trail]    -> Text)
  ([[Trail]]  -> Text)
outline2Renderer = outline1Renderer
  { outlineTitle = renderTitle
  , outlineAuthor = renderAuthor
  }

mapAppendEndline :: [Text] -> Text
mapAppendEndline = Text.concat . fmap (flip Text.append "\n")

addFinalEndline :: Text -> Text
addFinalEndline input = Text.append input "\n"

renderTrails :: [Trail] -> Text
renderTrails = Text.intercalate "\n" . fmap renderTrail

renderTrail :: Trail -> Text
renderTrail (Trail (Line _ text) blanks) = Text.concat $ text : makeBlankLines blanks

makeBlankLines :: Int -> [Text]
makeBlankLines blanks = replicate blanks "\n"

parseActsTrail :: [Trail] -> [[Trail]]
parseActsTrail [] = []
parseActsTrail input@(_ : _) =
  let (current, after) = takeWhileExtraSplit ((<= 1) . trailBlankLines) input
  in case after of
    [] -> [current]
    xs@(_ : _) -> let results = parseActsTrail xs in current : results

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
