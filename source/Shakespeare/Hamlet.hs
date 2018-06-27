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

  outline <- eitherToError $ parseFull hamletText
  Text.Lazy.IO.putStrLn $ Formatting.format ("title: " % Formatting.stext) ((\(Title t) -> t) $ outlineTitle outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("author: " % Formatting.stext) ((\(Author t) -> t) $ outlineAuthor outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("content acts count: " % Formatting.shown) (length . (\(Contents acts) -> acts) $ outlineContents outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("actors count: " % Formatting.shown) (length . (\(Actors actors _) -> actors) $ outlineActors outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("acts count: " % Formatting.shown) (length $ outlineActs outline)

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

outlineContentsLens :: forall a b c c' d e f. Functor f => (c -> f c') -> OutlineV a b c d e -> f (OutlineV a b c' d e)
outlineContentsLens f (Outline a b c d e) = fmap (\c' -> Outline a b c' d e) (f c)

outlineActorsLens :: forall a b c d d' e f. Functor f => (d -> f d') -> OutlineV a b c d e -> f (OutlineV a b c d' e)
outlineActorsLens f (Outline a b c d e) = fmap (\d' -> Outline a b c d' e) (f d)

type Outline1 = OutlineV Trail Trail  [Trail]  [Trail] [[Trail]]
type Outline2 = OutlineV Title Author Contents Actors  [[Trail]]
type OutlineRenderer t a b c d e = OutlineV (a -> t) (b -> t) (c -> t) (d -> t) (e -> t)

data OutlineTrailError
  = OutlineNoTitle
  | OutlineNoAuthor
  deriving Show

newtype Title = Title Text
newtype Author = Author Text

data TitleError = TitleInvalidBlankLines Int deriving Show
data AuthorError
  = AuthorInvalidBlankLines Int
  | AuthorNoBy Text
  deriving Show

data AllError
  = AllErrorInitialBlankLine  InitialBlankLine
  | AllErrorOutlineTrail      OutlineTrailError
  | AllErrorTitle             TitleError
  | AllErrorAuthor            AuthorError
  | AllErrorContents          ContentsError
  | AllErrorActors            ActorsError
  deriving Show

parseFull :: Text -> Either AllError Outline2
parseFull input
  =  (Lens.over Lens._Left AllErrorInitialBlankLine . getTrails . getTextLines $ input)
  >>= Lens.over Lens._Left AllErrorOutlineTrail     . parseOutline1
  >>= Lens.over Lens._Left AllErrorTitle            . outlineTitleLens parseTitle
  >>= Lens.over Lens._Left AllErrorAuthor           . outlineAuthorLens parseAuthor
  >>= Lens.over Lens._Left AllErrorContents         . outlineContentsLens parseContents
  >>= Lens.over Lens._Left AllErrorActors           . outlineActorsLens parseActors

data Actors = Actors
  { actorsList :: [Actor]
  , actorsScene :: Trail
  }

data Actor = Actor
  { actorTrail :: Trail
  }

data ActorsError
  = EmptyActors
  | InvalidActorHeader Trail
  | MissingSceneAfterActors
  | TooManyScenesAfterActors [Trail]
  deriving Show

parseActors :: [Trail] -> Either ActorsError Actors
parseActors input = do
  (header, afterHeader) <-
    case input of
      [] -> Left EmptyActors
      x : xs -> Right (x, xs)
  case header of
    Trail (Line _ t) 1 | t == Text.append " " actorsHeaderText -> Right ()
    t -> Left $ InvalidActorHeader t

  let
    (actorTrails, lastTrails) = takeWhileExtraSplit ((== 0) . trailBlankLines) afterHeader
    actors = fmap Actor actorTrails
  scene <-
    case lastTrails of
      [] -> Left MissingSceneAfterActors
      [x] -> Right x
      xs -> Left $ TooManyScenesAfterActors xs
  Right $ Actors actors scene

actorsHeaderText :: Text
actorsHeaderText = "Dramatis Personæ"

renderActors :: Actors -> Text
renderActors (Actors actors scene) =
  Text.intercalate "\n"
    [ Text.concat [" ", actorsHeaderText, "\n"]
    , Text.intercalate "\n" (fmap renderActor actors)
    , renderTrail scene
    ]

renderActor :: Actor -> Text
renderActor (Actor t) = renderTrail t

data ContentsError
  = EmptyContents
  | InvalidContentsHeader Trail
  deriving Show

newtype Contents = Contents [ContentsAct]
newtype ContentsAct = ContentsAct [Trail]

parseContents :: [Trail] -> Either ContentsError Contents
parseContents input = do
  (header, afterHeader) <-
    case input of
      [] -> Left EmptyContents
      x : xs -> Right (x, xs)

  case header of
    Trail (Line _ "Contents") 1 -> Right ()
    t -> Left $ InvalidContentsHeader t

  let acts = fmap ContentsAct $ repeatTakeWhileExtraSplit ((== 0) . trailBlankLines) afterHeader
  Right $ Contents acts

renderContents :: Contents -> Text
renderContents (Contents acts) =
  Text.concat
    [ "Contents\n\n"
    , Text.intercalate "\n" (fmap renderContentsAct acts)
    ]

renderContentsAct :: ContentsAct -> Text
renderContentsAct (ContentsAct trails) = renderTrails trails

parseAuthor :: Trail -> Either AuthorError Author
parseAuthor (Trail (Line _ t) c) = do
  line <-
    if c == authorBlankLineCount
      then Right t
      else Left $ AuthorInvalidBlankLines c
  result <-
    case Text.stripPrefix "by " line of
      Nothing -> Left $ AuthorNoBy line
      Just x -> Right x
  Right $ Author result

authorBlankLineCount :: Int
authorBlankLineCount = 6

renderAuthor :: Author -> Text
renderAuthor (Author t) = Text.concat $
  [ "by "
  , t
  , makeBlankLines authorBlankLineCount
  ]

parseTitle :: Trail -> Either TitleError Title
parseTitle (Trail (Line _ t) c) =
  if c == titleBlankLineCount
    then Right $ Title t
    else Left $ TitleInvalidBlankLines c

titleBlankLineCount :: Int
titleBlankLineCount = 3

renderTitle :: Title -> Text
renderTitle (Title t) = Text.append t $ makeBlankLines titleBlankLineCount

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
  :: OutlineRenderer Text a b c d e
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

outline1Renderer :: OutlineRenderer Text Trail Trail [Trail] [Trail] [[Trail]]
outline1Renderer =
  Outline
    renderTrail
    renderTrail
    renderTrails
    renderTrails
    (Text.intercalate endline . fmap renderTrails)

fullRenderer :: OutlineRenderer Text Title Author Contents Actors [[Trail]]
fullRenderer = outline1Renderer
  { outlineTitle = renderTitle
  , outlineAuthor = renderAuthor
  , outlineContents = renderContents
  , outlineActors = renderActors
  }

endline :: Text
endline = "\n"

mapAppendEndline :: [Text] -> Text
mapAppendEndline = Text.concat . fmap (flip Text.append endline)

addFinalEndline :: Text -> Text
addFinalEndline input = Text.append input endline

renderTrails :: [Trail] -> Text
renderTrails = Text.intercalate endline . fmap renderTrail

renderTrail :: Trail -> Text
renderTrail (Trail (Line _ text) blanks) = Text.append text $ makeBlankLines blanks

makeBlankLines :: Int -> Text
makeBlankLines blanks = Text.replicate blanks endline

parseActsTrail :: [Trail] -> [[Trail]]
parseActsTrail = repeatTakeWhileExtraSplit ((<= 1) . trailBlankLines)

repeatTakeWhileExtraSplit :: (a -> Bool) -> [a] -> [[a]]
repeatTakeWhileExtraSplit _ [] = []
repeatTakeWhileExtraSplit f input@(_ : _) =
  let (current, after) = takeWhileExtraSplit f input
  in case after of
    [] -> [current]
    xs@(_ : _) -> let results = repeatTakeWhileExtraSplit f xs in current : results

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
