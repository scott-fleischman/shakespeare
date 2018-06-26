{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Monad.IO.Class as Monad.IO.Class
import qualified Data.Algorithm.Diff as Algorithm.Diff
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
-- import           Hedgehog ((===))
import qualified Hedgehog
import qualified Shakespeare.Hamlet

prop_parseRender :: Hedgehog.Property
prop_parseRender =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    hamletText <- Monad.IO.Class.liftIO $ Text.IO.readFile "hamlet.txt"
    let hamletLines = Shakespeare.Hamlet.getTextLines hamletText
    trails <- Monad.IO.Class.liftIO $ Shakespeare.Hamlet.eitherToError . Shakespeare.Hamlet.getTrails $ hamletLines
    outline <- Monad.IO.Class.liftIO $ Shakespeare.Hamlet.eitherToError $ Shakespeare.Hamlet.parseOutline2 trails

    let
      lineNumbers = [1..] :: [Int]
      diffRaw =
        Algorithm.Diff.getDiffBy (\a b -> snd a == snd b)
          (zip lineNumbers . Text.lines $ Shakespeare.Hamlet.renderOutlineV Shakespeare.Hamlet.outline2Renderer outline)
          (zip lineNumbers . Text.lines $ hamletText)
      notBoth (Algorithm.Diff.Both _ _) = False
      notBoth _ = True
      diff = filter notBoth diffRaw
    case diff of
      [] -> return ()
      (_ : _) -> do
        mapM_ Hedgehog.footnoteShow $ reverse diff
        Hedgehog.failure

tests :: IO Bool
tests = Hedgehog.checkParallel $$(Hedgehog.discover)

main :: IO ()
main = do
  success <- tests
  if success
    then return ()
    else fail "Test failure"
