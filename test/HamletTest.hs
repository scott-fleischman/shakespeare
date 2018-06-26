{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Monad.IO.Class as Monad.IO.Class
import           Hedgehog ((===))
import qualified Hedgehog
import qualified Shakespeare.Hamlet
import qualified Data.Text.IO as Text.IO

prop_parseRender :: Hedgehog.Property
prop_parseRender =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    hamletText <- Monad.IO.Class.liftIO $ Text.IO.readFile "hamlet.txt"
    let hamletLines = Shakespeare.Hamlet.getTextLines hamletText
    trails <- Monad.IO.Class.liftIO $ Shakespeare.Hamlet.eitherToError . Shakespeare.Hamlet.getTrails $ hamletLines
    outline <- Monad.IO.Class.liftIO $ Shakespeare.Hamlet.eitherToError $ Shakespeare.Hamlet.parseOutline trails
    Shakespeare.Hamlet.renderOutline outline === hamletText

tests :: IO Bool
tests = Hedgehog.checkParallel $$(Hedgehog.discover)

main :: IO ()
main = do
  success <- tests
  if success
    then return ()
    else fail "Test failure"
