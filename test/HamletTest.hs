{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Monad.IO.Class as Monad.IO.Class
import qualified Data.Algorithm.Diff as Algorithm.Diff
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Hedgehog ((===))
import qualified Hedgehog
import qualified Shakespeare.Hamlet

prop_parseRender :: Hedgehog.Property
prop_parseRender =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    hamletText <- Monad.IO.Class.liftIO $ Text.IO.readFile "hamlet.txt"
    outline <- Monad.IO.Class.liftIO $ Shakespeare.Hamlet.eitherToError $ Shakespeare.Hamlet.parseFull hamletText
    Shakespeare.Hamlet.renderOutlineV Shakespeare.Hamlet.fullRenderer outline
      `linesShouldEqual`
      hamletText

prop_takeWhileExtraSplit_examples :: Hedgehog.Property
prop_takeWhileExtraSplit_examples =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    Shakespeare.Hamlet.takeWhileExtraSplit Char.isLower "" === ("", "")
    Shakespeare.Hamlet.takeWhileExtraSplit Char.isLower "abc" === ("abc", "")
    Shakespeare.Hamlet.takeWhileExtraSplit Char.isLower "abcD" === ("abcD", "")
    Shakespeare.Hamlet.takeWhileExtraSplit Char.isLower "abcDefghI" === ("abcD", "efghI")
    Shakespeare.Hamlet.takeWhileExtraSplit Char.isLower "Abc" === ("A", "bc")

prop_takeFirstUntil_examples :: Hedgehog.Property
prop_takeFirstUntil_examples =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    Shakespeare.Hamlet.takeFirstUntil Char.isUpper "" === ("", "")
    Shakespeare.Hamlet.takeFirstUntil Char.isUpper "A" === ("A", "")
    Shakespeare.Hamlet.takeFirstUntil Char.isUpper "Abc" === ("Abc", "")
    Shakespeare.Hamlet.takeFirstUntil Char.isUpper "AbcDef" === ("Abc", "Def")
    Shakespeare.Hamlet.takeFirstUntil Char.isUpper "qAbcDef" === ("", "qAbcDef")

prop_takeWhileSplit_examples :: Hedgehog.Property
prop_takeWhileSplit_examples =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    Shakespeare.Hamlet.takeWhileSplit Char.isLower "" === ("", "")
    Shakespeare.Hamlet.takeWhileSplit Char.isLower "abc" === ("abc", "")
    Shakespeare.Hamlet.takeWhileSplit Char.isLower "abcDef" === ("abc", "Def")
    Shakespeare.Hamlet.takeWhileSplit Char.isLower "AbcDef" === ("", "AbcDef")

linesShouldEqual :: Text.Text -> Text.Text -> Hedgehog.PropertyT IO ()
linesShouldEqual actualText expectedText = do
  let
    lineNumbers = [1..] :: [Int]
    makeLines = zip lineNumbers . Text.lines
    diffRaw =
      Algorithm.Diff.getDiffBy
        (\a b -> snd a == snd b)
        (makeLines actualText)
        (makeLines expectedText)

    notBoth (Algorithm.Diff.Both _ _) = False
    notBoth _ = True

    diff
      = take 30
      . filter notBoth
      $ diffRaw
  case diff of
    [] -> return ()
    _ : _ -> do
      let
        footnoteDiff (Algorithm.Diff.First (ln, t))  = Hedgehog.footnote $ "Actual:   Line " ++ show ln ++ ": " ++ Text.unpack t
        footnoteDiff (Algorithm.Diff.Second (ln, t)) = Hedgehog.footnote $ "Expected: Line " ++ show ln ++ ": " ++ Text.unpack t
        footnoteDiff (Algorithm.Diff.Both _ _) = return ()
      mapM_ footnoteDiff $ reverse diff
      Hedgehog.failure

tests :: IO Bool
tests = Hedgehog.checkParallel $$(Hedgehog.discover)

main :: IO ()
main = do
  success <- tests
  if success
    then return ()
    else fail "Test failure"
