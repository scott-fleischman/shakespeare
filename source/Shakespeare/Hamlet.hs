{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Shakespeare.Hamlet where

import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Monad ((>=>))
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import           Data.Generics.Product (field, position, typed)
import           Data.Generics.Sum (_Ctor, _Typed)
import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import           Formatting ((%), (%.))
import qualified Formatting
import           GHC.Generics (Generic)
import           Prelude hiding (lines)
import qualified Shakespeare.Twitter as Twitter
import qualified Text.Numeral.Roman

main :: IO ()
main = do
  hamletText <- Text.IO.readFile "hamlet.txt"

  outline <- eitherToError $ parseFull hamletText
  Text.Lazy.IO.putStrLn $ Formatting.format ("title: " % Formatting.stext) ((\(Title t) -> t) $ outlineTitle outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("author: " % Formatting.stext) ((\(Author t) -> t) $ outlineAuthor outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("content acts count: " % Formatting.shown) (length . (\(Contents acts) -> acts) $ outlineContents outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("actors count: " % Formatting.shown) (length . (\(Actors actors _) -> actors) $ outlineActors outline)
  Text.Lazy.IO.putStrLn $ Formatting.format ("acts count: " % Formatting.shown) (length $ outlineActs outline)

  let book = makeTwitterBook outline
  printTwitterBookStats book
  Aeson.encodeFile "hamlet-twitter-book.json" book

printTwitterBookStats :: Twitter.Book -> IO ()
printTwitterBookStats book = do
  Formatting.fprint ("Twitter thread count: " % Formatting.shown % "\n") (length $ book ^. typed @[Twitter.Thread])
  Formatting.fprint ("Total tweet count: " % Formatting.shown % "\n")
    (length $ Lens.toListOf (typed @[Twitter.Thread] . traverse . typed @[Twitter.Tweet] . traverse) book)

makeTwitterBook :: Outline3 -> Twitter.Book
makeTwitterBook = Twitter.Book . concatMap makeActThread . (Lens.view $ typed @[Act2])

makeActThread :: Act2 -> [Twitter.Thread]
makeActThread act = fmap (makeSceneThread act) $ act ^. typed @[Scene2]

narratorName :: Text
narratorName = "narrator"

makeSceneThread :: Act2 -> Scene2 -> Twitter.Thread
makeSceneThread act scene =
  Twitter.Thread
    $ Twitter.Tweet narratorName sceneTweet
    : concatMap makeSceneItemTweets (scene ^. typed @[SceneItem2])
  where
  sceneTweet = Formatting.sformat
    ( "#Hamlet Act " % Formatting.stext % ".\n"
    % "Scene " % Formatting.stext % ". " % Formatting.stext
    )
    (Text.Numeral.Roman.toRoman (act ^. typed @ActNumber . typed @Int))
    (Text.Numeral.Roman.toRoman (scene ^. typed @SceneNumber . typed @Int))
    (scene ^. typed @SceneDescription . typed @Text)

makeSceneItemTweets :: SceneItem2 -> [Twitter.Tweet]
makeSceneItemTweets (SceneItem2Note note) =
  fmap
    (Twitter.Tweet narratorName)
    (breakLinesIntoTweets $ Lens.toListOf (traverse . typed @Line . typed @Text) note)
makeSceneItemTweets (SceneItem2NamedDialog (NamedDialog actor lines)) =
  fmap
    (Twitter.Tweet $ getTweetAuthor $ actor)
    (breakLinesIntoTweets $ Lens.toListOf (traverse . typed @Line . typed @Text) lines)

getTweetAuthor :: DialogActor -> Text
getTweetAuthor (SingleActor x) = Text.toLower x
getTweetAuthor AllActors = narratorName
getTweetAuthor (TwoActors x _) = Text.toLower x

maxTweetLength :: Int
maxTweetLength = 240

breakLinesIntoTweets :: [Text] -> [Text]
breakLinesIntoTweets [] = []
breakLinesIntoTweets (x : xs) = go x xs
  where
  go :: Text -> [Text] -> [Text]
  go soFar [] = [soFar]
  go soFar (y : ys) =
    let extra = Text.concat [soFar, "\n", y]
    in if Text.length extra > maxTweetLength
      then soFar : go y ys
      else go extra ys

writeActorLineCounts :: Outline3 -> IO ()
writeActorLineCounts outline = do
  let
    namedDialogs
      = Lens.toListOf
        ( typed @[Act2]
        . traverse
        . typed @[Scene2]
        . traverse
        . typed @[SceneItem2]
        . traverse
        . _Ctor @"SceneItem2NamedDialog"
        )
        $ outline
    toPair (NamedDialog actor lines) = (actor, length lines)
    actorLineCounts = Map.toList . Map.fromListWith (+) $ fmap toPair namedDialogs
    descendingActors = List.sortBy (Ord.comparing $ Ord.Down . snd) actorLineCounts
    output = Text.Lazy.unlines $
      fmap
        (\(actor, lines) ->
          Formatting.format
            ((Formatting.right 30 ' ' %. Formatting.stext) % Formatting.shown)
          (renderDialogActor actor)
          lines
        )
        descendingActors
  Text.Lazy.IO.writeFile "hamlet-line-counts.txt" output

writeDialogDebugFiles :: Outline2 -> IO ()
writeDialogDebugFiles outline = do
  let
    acts = outlineActs outline

    actors
      = Lens.toListOf (traverse . _Typed @NamedDialog . typed @DialogActor)
      . flattenSceneItems
      $ acts
    singleNames = Set.toList . Set.fromList . Lens.toListOf (traverse . _Ctor @"SingleActor") $ actors
    doubleNames = Set.toList . Set.fromList . Lens.toListOf (traverse . _Ctor @"TwoActors") $ actors
    namesText =
      Text.concat
        [ Text.unlines singleNames
        , "\n"
        , Text.Lazy.toStrict . Text.Lazy.unlines $
            fmap
              (uncurry $ Formatting.format ((Formatting.right 15 ' ' %. Formatting.stext) % Formatting.stext))
              doubleNames
        ]
  Text.IO.writeFile "hamlet-dialog-names.txt" namesText

  let
    notes = getSceneNotes acts
    formattedNotes = Text.Lazy.unlines $ formatTrails notes
  Text.Lazy.IO.writeFile "hamlet-dialog-notes.txt" formattedNotes

  let
    unnamedDialog = getSceneUnnamedDialog acts
    formattedUnnamedDialog = Text.Lazy.unlines $ formatTrails unnamedDialog
  Text.Lazy.IO.writeFile "hamlet-dialog-unnamed.txt" formattedUnnamedDialog

  let
    namedDialog = getSceneNamedDialog acts
    namedDialogPunctuation =
      filter
        (any
          ( not
          . Text.all
            (\c
              -> Char.isLetter c
              || Char.isSpace c
              || c == '‘' || c == '’'
              || c == '.' || c == '?' || c == '!'
              || c == ',' || c == ';' || c == ':'
              || c == '—' || c == '-'
              || c == '_'
            )
          . lineText
          . trailLine
          )
        )
        namedDialog
    formattedNamedDialog = Text.Lazy.unlines $ formatTrails namedDialogPunctuation
  Text.Lazy.IO.writeFile "hamlet-dialog-named-punctuation.txt" formattedNamedDialog

getSceneNotes :: [Act] -> [[Trail]]
getSceneNotes = Lens.toListOf (traverse . _Ctor @"SceneItemUnnamedNote") . flattenSceneItems

getSceneUnnamedDialog :: [Act] -> [[Trail]]
getSceneUnnamedDialog = Lens.toListOf (traverse . _Ctor @"SceneItemUnnamedUnnamedDialog") . flattenSceneItems

getSceneNamedDialog :: [Act] -> [[Trail]]
getSceneNamedDialog = Lens.toListOf (traverse . _Ctor @"SceneItemUnnamedNamedDialog" . typed @[Trail]) . flattenSceneItems

formatTrails :: [[Trail]] -> [Text.Lazy.Text]
formatTrails = fmap (flip Text.Lazy.append "\n" . Text.Lazy.intercalate "\n" . fmap (formatLine . trailLine))

flattenSceneItems :: [Act] -> [SceneItemUnnamed]
flattenSceneItems = Lens.toListOf (traverse . typed @[Scene] . traverse . typed @[SceneItemUnnamed] . traverse)

formatAct :: Act -> Text.Lazy.Text
formatAct act = Formatting.format
  ("Act " % Formatting.int % " : " % Formatting.int % " scenes\n" % Formatting.text)
  (act ^. typed @ActNumber . typed @Int)
  (length $ act ^. typed @[Scene])
  (Text.Lazy.concat . fmap (Formatting.format ("  " % Formatting.text % "\n") . formatScene) $ act ^. typed @[Scene])

formatScene :: Scene -> Text.Lazy.Text
formatScene scene =
  Formatting.format
    ("Scene " % Formatting.int % ". " % Formatting.stext % "\n  " % Formatting.text)
    (scene ^. typed @SceneNumber . typed @Int)
    (scene ^. typed @SceneDescription . typed @Text)
    (Text.Lazy.intercalate "\n" $ fmap (Formatting.format ("  " % Formatting.shown)) $ take 5 $ scene ^. typed @[SceneItemUnnamed])

formatActor :: Actor -> Text.Lazy.Text
formatActor (ActorMajor (ActorLabel label) t) = Formatting.format (Formatting.right 13 ' ' % Formatting.stext % " ") label (renderTrail t)
formatActor (ActorMinor t)                    = Formatting.format (Formatting.right 13 ' ' % Formatting.stext % " ") ("(minor)" :: Text) (renderTrail t)

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
  } deriving (Show, Generic)

outlineTitleLens    :: forall a a' b c d e f. Functor f => (a -> f a') -> OutlineV a b c d e -> f (OutlineV a' b c d e)
outlineTitleLens    f (Outline a b c d e) = fmap (\a' -> Outline a' b c d e) (f a)

outlineAuthorLens   :: forall a b b' c d e f. Functor f => (b -> f b') -> OutlineV a b c d e -> f (OutlineV a b' c d e)
outlineAuthorLens   f (Outline a b c d e) = fmap (\b' -> Outline a b' c d e) (f b)

outlineContentsLens :: forall a b c c' d e f. Functor f => (c -> f c') -> OutlineV a b c d e -> f (OutlineV a b c' d e)
outlineContentsLens f (Outline a b c d e) = fmap (\c' -> Outline a b c' d e) (f c)

outlineActorsLens   :: forall a b c d d' e f. Functor f => (d -> f d') -> OutlineV a b c d e -> f (OutlineV a b c d' e)
outlineActorsLens   f (Outline a b c d e) = fmap (\d' -> Outline a b c d' e) (f d)

outlineActsLens     :: forall a b c d e e' f. Functor f => (e -> f e') -> OutlineV a b c d e -> f (OutlineV a b c d e')
outlineActsLens     f (Outline a b c d e) = fmap (\e' -> Outline a b c d e') (f e)

type Outline1 = OutlineV Trail Trail  [Trail]  [Trail] [[Trail]]
type Outline2 = OutlineV Title Author Contents Actors  [Act]
type Outline3 = OutlineV Title Author Contents Actors  [Act2]
type OutlineRenderer t a b c d e = OutlineV (a -> t) (b -> t) (c -> t) (d -> t) (e -> t)

data OutlineTrailError
  = OutlineNoTitle
  | OutlineNoAuthor
  deriving (Show, Generic)

newtype Title = Title Text deriving Generic
newtype Author = Author Text deriving Generic

data TitleError = TitleInvalidBlankLines Int deriving (Show, Generic)
data AuthorError
  = AuthorInvalidBlankLines Int
  | AuthorNoBy Text
  deriving (Show, Generic)

data AllError
  = AllErrorInitialBlankLine  InitialBlankLine
  | AllErrorOutlineTrail      OutlineTrailError
  | AllErrorTitle             TitleError
  | AllErrorAuthor            AuthorError
  | AllErrorContents          ContentsError
  | AllErrorActors            ActorsError
  | AllErrorAct               ActError
  | AllErrorDialogNamingError DialogNamingError
  deriving (Show, Generic)

parseFull :: Text -> Either AllError Outline3
parseFull
  =   overLeft AllErrorInitialBlankLine   . getTrails . getTextLines
  >=> overLeft AllErrorOutlineTrail       . parseOutline1
  >=> overLeft AllErrorTitle              . outlineTitleLens parseTitle
  >=> overLeft AllErrorAuthor             . outlineAuthorLens parseAuthor
  >=> overLeft AllErrorContents           . outlineContentsLens parseContents
  >=> overLeft AllErrorActors             . outlineActorsLens parseActors
  >=> overLeft AllErrorAct                . outlineActsLens parseActs
  >=> overLeft AllErrorDialogNamingError  . (outlineActsLens . traverse . position @2 . traverse . position @3) nameUnnamedDialog

overLeft :: (a -> a') -> Either a b -> Either a' b
overLeft f (Left x)   = Left (f x)
overLeft _ (Right x)  = Right x

data ActError
  = MissingActHeader
  | InvalidActHeaderPrefix Trail
  | InvalidActHeaderBlankLines Trail
  | InvalidActNumber Trail
  | ActSceneError SceneError
  deriving (Show, Generic)

parseActs :: [[Trail]] -> Either ActError [Act]
parseActs = traverse parseAct

actHeaderText :: Text
actHeaderText = "ACT"

parseAct :: [Trail] -> Either ActError Act
parseAct input = do
  (headerTrail, afterHeader) <-
    case input of
      [] -> Left MissingActHeader
      x : xs -> Right (x, xs)
  numberText <-
    case Text.stripPrefix (Text.append actHeaderText " ") (lineText . trailLine $ headerTrail) of
      Nothing -> Left $ InvalidActHeaderPrefix headerTrail
      Just x -> Right x
  if trailBlankLines headerTrail == 1
    then Right ()
    else Left $ InvalidActHeaderBlankLines headerTrail
  numberValue <-
    case Text.Numeral.Roman.fromRoman numberText of
      Nothing -> Left $ InvalidActNumber headerTrail
      Just x -> Right x
  let
    sceneTrails :: [[Trail]]
    sceneTrails = repeatSplit (takeFirstUntil isSceneHeader) afterHeader

  scenes :: [Scene]
    <- overLeft ActSceneError $ traverse parseScene sceneTrails
  Right $ Act (ActNumber numberValue) scenes

isSceneHeader :: Trail -> Bool
isSceneHeader = Text.isPrefixOf (Text.append scenePrefixText " ") . lineText . trailLine

scenePrefixText :: Text
scenePrefixText = "SCENE"

renderActs :: [Act] -> Text
renderActs = renderActsV (renderSceneV $ Text.intercalate "\n" . fmap renderSceneItemUnnamed)

renderActsV :: forall a. (SceneV a -> Text) -> [ActV [SceneV a]] -> Text
renderActsV renderSceneF = Text.intercalate "\n" . fmap (renderActV renderSceneF)

renderActV :: forall a. (SceneV a -> Text) -> ActV [SceneV a] -> Text
renderActV renderSceneF act = Text.concat
  [ actHeaderText
  , " "
  , Text.Numeral.Roman.toRoman $ act ^. typed @ActNumber . typed @Int
  , "\n\n"
  , Text.intercalate "\n" . fmap renderSceneF $ act ^. typed @[SceneV a]
  ]

data SceneError
  = MissingSceneHeader
  | MissingScenePrefix Trail
  | InvalidSceneNumber Text Trail
  | InvalidScenePunctutationAfterNumber Text Trail
  | SceneErrorItem SceneItemError
  deriving (Show, Generic)

parseScene :: [Trail] -> Either SceneError Scene
parseScene input = do
  (headerTrail, afterHeader) <-
    case input of
      [] -> Left MissingSceneHeader
      x : xs -> Right (x, xs)
  numberRest <-
    case Text.stripPrefix (Text.append scenePrefixText " ") (lineText . trailLine $ headerTrail) of
      Nothing -> Left $ MissingScenePrefix headerTrail
      Just x -> Right x
  let (numberText, afterNumber) = Text.breakOn "." numberRest
  number <-
    case Text.Numeral.Roman.fromRoman numberText of
      Nothing -> Left $ InvalidSceneNumber numberText headerTrail
      Just x -> Right $ SceneNumber x
  description <-
    case Text.stripPrefix ". " afterNumber of
      Nothing -> Left $ InvalidScenePunctutationAfterNumber afterNumber headerTrail
      Just x -> Right $ SceneDescription x
  let sceneItemTrails = repeatSplit (takeWhileExtraSplit ((== 0) . trailBlankLines)) afterHeader
  items <-
    overLeft SceneErrorItem $ traverse parseSceneItem sceneItemTrails
  Right $ Scene number description items

renderSceneV :: forall a. (a -> Text) -> SceneV a -> Text
renderSceneV renderSceneItemsF scene =
  Text.concat
    [ scenePrefixText
    , " "
    , Text.Numeral.Roman.toRoman $ scene ^. position @1 . typed @Int
    , ". "
    , scene ^. position @2 . typed @Text
    , "\n\n"
    , renderSceneItemsF $ scene ^. position @3
    ]

data SceneItemError
  = EmptySceneItem
  | InvalidSingletonSceneItem Trail
  | InvalidDialogActor Line
  deriving (Show, Generic)

parseSceneItem :: [Trail] -> Either SceneItemError SceneItemUnnamed
parseSceneItem [] = Left EmptySceneItem
parseSceneItem (single : []) =
  let line = (lineText . trailLine) single
  in case Text.stripPrefix " " line of
    Nothing -> Right $ SceneItemUnnamedUnnamedDialog [single]
    Just _ -> Right $ SceneItemUnnamedNote [single]
parseSceneItem (initial : rest) =
  let initialText = lineText . trailLine $ initial
  in if Text.isPrefixOf " " initialText
    then
      if isPoetry $ fmap (Lens.view $ field @"trailLine" . field @"lineText") (initial : rest)
        then Right $ SceneItemUnnamedUnnamedDialog (initial : rest)
        else Right $ SceneItemUnnamedNote (initial : rest)
    else
      let isCapsWord = Text.all (\x -> Char.isUpper x || x == '.')
      in if (all (\x -> x == "and" || isCapsWord x) . Text.words) initialText
        then do
          actor <- parseDialogActor $ initial ^. typed @Line
          Right $ SceneItemUnnamedNamedDialog $ NamedDialog actor rest
        else Right $ SceneItemUnnamedUnnamedDialog (initial : rest)

parseDialogActor :: Line -> Either SceneItemError DialogActor
parseDialogActor line@(Line _ raw) = do
  simple <-
    case Text.stripSuffix "." raw of
      Nothing -> Left $ InvalidDialogActor line
      Just x -> return x
  case Text.words simple of
    [t]
      | Text.toLower t == "all" -> Right $ AllActors
      | otherwise -> Right $ SingleActor t
    [_, _] -> Right $ SingleActor simple
    [w1, w2, w3] | Text.toLower w2 == "and" -> Right $ TwoActors w1 w3
    _ -> Left $ InvalidDialogActor line

isPoetry :: [Text] -> Bool
isPoetry = isPoeticIndent . Set.fromList . fmap (Text.length . Text.takeWhile Char.isSpace)
  where
  isPoeticIndent indentLengths
    = Set.size indentLengths >= 2
    && Foldable.all (> 0) indentLengths

renderSceneItemUnnamed :: SceneItemUnnamed -> Text
renderSceneItemUnnamed (SceneItemUnnamedNote note) = renderTrails note
renderSceneItemUnnamed (SceneItemUnnamedUnnamedDialog lines) = renderTrails lines
renderSceneItemUnnamed (SceneItemUnnamedNamedDialog (NamedDialog actor lines)) = Text.concat [renderDialogActor actor, ".\n", renderTrails (lines)]

renderSceneItem2List :: [SceneItem2] -> Text
renderSceneItem2List = Text.intercalate "\n" . fmap renderSceneItemUnnamed . sceneItemNamedToUnnamed

sceneItemNamedToUnnamed :: [SceneItem2] -> [SceneItemUnnamed]
sceneItemNamedToUnnamed = go Nothing
  where
  go _ [] = []
  go (Just actor1)
    ( SceneItem2NamedDialog (NamedDialog actor2 lines2)
    : xs
    )
    | actor1 == actor2
    = SceneItemUnnamedUnnamedDialog lines2
    : go (Just actor1) xs
  go _ (x@(SceneItem2NamedDialog (NamedDialog actor _)) : xs) = simpleNamedToUnnamed x : go (Just actor) xs
  go a (x@(SceneItem2Note _) : xs) = simpleNamedToUnnamed x : go a xs

simpleNamedToUnnamed :: SceneItem2 -> SceneItemUnnamed
simpleNamedToUnnamed (SceneItem2Note note) = SceneItemUnnamedNote note
simpleNamedToUnnamed (SceneItem2NamedDialog (NamedDialog actor lines)) = SceneItemUnnamedNamedDialog (NamedDialog actor lines)

renderSceneItem2 :: SceneItem2 -> Text
renderSceneItem2 (SceneItem2Note note) = renderTrails note
renderSceneItem2 (SceneItem2NamedDialog (NamedDialog actor lines)) = Text.concat [renderDialogActor actor, ".\n", renderTrails (lines)]

renderDialogActor :: DialogActor -> Text
renderDialogActor AllActors = "ALL"
renderDialogActor (SingleActor t) = t
renderDialogActor (TwoActors a1 a2) = Text.concat [a1, " and ", a2]

data DialogNamingError
  = UnnamedDialogWithoutPreviousNamedDialog [Trail]
  deriving Show

nameUnnamedDialog :: [SceneItemUnnamed] -> Either DialogNamingError [SceneItem2]
nameUnnamedDialog = go Nothing
  where
  go _ [] = Right []
  go s (SceneItemUnnamedNote x : xs) = go s xs >>= Right . (SceneItem2Note x :)
  go _ (SceneItemUnnamedNamedDialog x@(NamedDialog a _) : xs) = go (Just a) xs >>= Right . (SceneItem2NamedDialog x :)
  go Nothing (SceneItemUnnamedUnnamedDialog x : _) = Left $ UnnamedDialogWithoutPreviousNamedDialog x
  go (Just a) (SceneItemUnnamedUnnamedDialog x : xs) = go (Just a) xs >>= Right . (SceneItem2NamedDialog (NamedDialog a x) :)

newtype ActNumber = ActNumber Int deriving Generic
data ActV a = Act ActNumber a deriving Generic
type Act = ActV [Scene]
type Act2 = ActV [Scene2]

newtype SceneNumber = SceneNumber Int deriving Generic
newtype SceneDescription = SceneDescription Text deriving Generic
data SceneV a = Scene SceneNumber SceneDescription a deriving Generic
type Scene = SceneV [SceneItemUnnamed]
type Scene2 = SceneV [SceneItem2]

data SceneItemUnnamed
  = SceneItemUnnamedNote [Trail]
  | SceneItemUnnamedNamedDialog NamedDialog
  | SceneItemUnnamedUnnamedDialog [Trail]
  deriving (Generic, Show)

data SceneItem2
  = SceneItem2Note [Trail]
  | SceneItem2NamedDialog NamedDialog
  deriving (Generic, Show)

data NamedDialog = NamedDialog DialogActor [Trail] deriving (Generic, Show)

data Actors = Actors
  { actorsList :: [Actor]
  , actorsScene :: Trail
  }

newtype ActorLabel = ActorLabel Text deriving (Eq, Ord, Generic)

data DialogActor
  = SingleActor Text
  | TwoActors Text Text
  | AllActors
  deriving (Eq, Generic, Ord, Show)

data Actor
  = ActorMajor ActorLabel Trail
  | ActorMinor Trail
  deriving Generic

data ActorsError
  = EmptyActors
  | InvalidActorHeader Trail
  | MissingSceneAfterActors
  | TooManyScenesAfterActors [Trail]
  deriving (Generic, Show)

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
    actors = fmap parseActor actorTrails
  scene <-
    case lastTrails of
      [] -> Left MissingSceneAfterActors
      [x] -> Right x
      xs -> Left $ TooManyScenesAfterActors xs
  Right $ Actors actors scene

parseActor :: Trail -> Actor
parseActor trail@(Trail (Line _ text) _) =
  case tryExtractActorLabel text of
    Nothing -> ActorMinor trail
    Just label -> ActorMajor label trail

tryExtractActorLabel :: Text -> Maybe ActorLabel
tryExtractActorLabel t
  | [label]
    <- filter ((>1) . Text.length)
    . filter (Text.all Char.isUpper)
    . fmap (Text.filter (/= ','))
    $ Text.words t
  = Just $ ActorLabel label
tryExtractActorLabel _ = Nothing

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
renderActor (ActorMajor _ t) = renderTrail t
renderActor (ActorMinor t) = renderTrail t

data ContentsError
  = EmptyContents
  | InvalidContentsHeader Trail
  deriving (Generic, Show)

newtype Contents = Contents [ContentsAct] deriving Generic
newtype ContentsAct = ContentsAct [Trail] deriving Generic

parseContents :: [Trail] -> Either ContentsError Contents
parseContents input = do
  (header, afterHeader) <-
    case input of
      [] -> Left EmptyContents
      x : xs -> Right (x, xs)

  case header of
    Trail (Line _ "Contents") 1 -> Right ()
    t -> Left $ InvalidContentsHeader t

  let acts = fmap ContentsAct $ repeatSplit (takeWhileExtraSplit $ (== 0) . trailBlankLines) afterHeader
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

fullRenderer :: OutlineRenderer Text Title Author Contents Actors [Act2]
fullRenderer = outline1Renderer
  { outlineTitle = renderTitle
  , outlineAuthor = renderAuthor
  , outlineContents = renderContents
  , outlineActors = renderActors
  , outlineActs = renderActsV (renderSceneV renderSceneItem2List)
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
parseActsTrail = repeatSplit $ takeWhileExtraSplit $ (<= 1) . trailBlankLines

repeatSplit :: ([a] -> ([a], [a])) -> [a] -> [[a]]
repeatSplit _ [] = []
repeatSplit f input@(_ : _) =
  let (current, after) = f input
  in case after of
    [] -> [current]
    xs@(_ : _) -> let results = repeatSplit f xs in current : results

takeWhileExtraSplit :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileExtraSplit _ [] = ([], [])
takeWhileExtraSplit f (x : xs) =
  if f x
    then let (r1, r2) = takeWhileExtraSplit f xs in (x : r1, r2)
    else ([x], xs)

takeFirstUntil :: (a -> Bool) -> [a] -> ([a], [a])
takeFirstUntil _ [] = ([], [])
takeFirstUntil f (x : xs) =
  if f x
    then let (r1, r2) = takeWhileSplit (not . f) xs in (x : r1, r2)
    else ([], x : xs)

takeWhileSplit :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileSplit _ [] = ([], [])
takeWhileSplit f (x : xs) =
  if f x
    then let (r1, r2) = takeWhileSplit f xs in (x : r1, r2)
    else ([], x : xs)

data Line = Line
  { lineNumber :: LineNumber
  , lineText :: Text
  } deriving (Generic, Show)
newtype LineNumber = LineNumber Int deriving (Generic, Show)

getTextLines :: Text -> [Line]
getTextLines = zipWith Line lineNumbers . Text.lines
  where
  lineNumbers = fmap LineNumber [1..]

data Trail = Trail
  { trailLine :: Line
  , trailBlankLines :: Int
  } deriving (Generic, Show)

data InitialBlankLine = InitialBlankLine deriving (Generic, Show)

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
