{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Monad.IO.Class as Monad.IO.Class
import           Hedgehog ((===))
import qualified Hedgehog
import qualified Shakespeare.Hamlet
import qualified Data.Text.IO as Text.IO

prop_parseRenderOutline1 :: Hedgehog.Property
prop_parseRenderOutline1 =
  Hedgehog.withTests 1 . Hedgehog.property $ do
    hamletText <- Monad.IO.Class.liftIO $ Text.IO.readFile "hamlet.txt"
    let hamletLines = Shakespeare.Hamlet.getTextLines hamletText
    trails <- Monad.IO.Class.liftIO $ Shakespeare.Hamlet.eitherToError . Shakespeare.Hamlet.getTrails $ hamletLines
    outline1 <- Monad.IO.Class.liftIO $ Shakespeare.Hamlet.eitherToError $ Shakespeare.Hamlet.parseOutline1 trails
    Shakespeare.Hamlet.renderOutline1 outline1 === hamletText

tests :: IO Bool
tests = Hedgehog.checkParallel $$(Hedgehog.discover)

main :: IO ()
main = do
  success <- tests
  if success
    then return ()
    else fail "Test failure"
