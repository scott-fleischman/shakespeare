{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

import qualified Shakespeare.Twitter
import qualified Control.Exception as Exception
import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import qualified Control.Logging as Logging
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import           Data.Generics.Product (field, typed)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Lazy as Map
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Formatting
import qualified Web.Twitter.Conduit as Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as Twitter.Conduit.Parameters
import qualified Web.Twitter.Types as Twitter.Types

main :: IO ()
main = Logging.withStdoutLogging $ do
  Logging.log "Loading config.json"
  config <- Aeson.eitherDecodeFileStrict @Shakespeare.Twitter.Config "config.json" >>= 
    \case
      Left err -> error err
      Right result -> return result

  let
    appInfo = config ^. field @"app"
    oauth =
      Twitter.Conduit.twitterOAuth
      { Twitter.Conduit.oauthConsumerKey    = Text.Encoding.encodeUtf8 $ appInfo ^. field @"key" 
      , Twitter.Conduit.oauthConsumerSecret = Text.Encoding.encodeUtf8 $ appInfo ^. field @"secret"
      }

    makeCred :: Shakespeare.Twitter.TokenInfo -> Twitter.Conduit.Credential
    makeCred tokenInfo =
      Twitter.Conduit.Credential
      [ ("oauth_token",         Text.Encoding.encodeUtf8 $ tokenInfo ^. field @"token")
      , ("oauth_token_secret",  Text.Encoding.encodeUtf8 $ tokenInfo ^. field @"secret")
      ]

    makeTWInfo = (\cred -> Twitter.Conduit.setCredential oauth cred Twitter.Conduit.def) . makeCred 
    nameToTWInfo = fmap makeTWInfo $ config ^. field @"accounts"

  Logging.log "Loading book.json"
  book <- Aeson.eitherDecodeFileStrict @Shakespeare.Twitter.Book "book.json" >>= 
    \case
      Left err -> error err
      Right result -> return result

  let
    allAuthors = Set.fromList $ Lens.toListOf (field @"threads" . traverse . field @"tweets" . traverse . field @"author") book
    authorMismatches = Set.filter (flip Map.notMember nameToTWInfo) allAuthors
  if Set.null authorMismatches
    then return ()
    else do
      Logging.log "Missing names in config:"
      mapM_ Logging.log $ Set.toList authorMismatches
      error "Missing names in config"

  _manager <- Twitter.Conduit.newManager Twitter.Conduit.tlsManagerSettings

  return ()
--   let timerConf = Async.Timer.timerConfSetInterval tweetInterval Async.Timer.defaultTimerConf
--   Async.Timer.withAsyncTimer timerConf $ \timer ->
--     Monad.forever $ do
--       randomTweets <- Random.Shuffle.shuffleM tweetThreads
--       Monad.forM_ randomTweets $ \thread -> do
--         Async.Timer.timerWait timer
--         tryPostThread manager thread

-- tryPostThread :: Twitter.Conduit.Manager -> Alice.Tweets.TweetThread -> IO ()
-- tryPostThread manager thread = do
--   possibleResult <- Exception.try @Exception.SomeException (postThread manager thread)
--   case possibleResult of
--     Left err -> Logging.warn (Text.pack . show $ err)
--     Right () -> return ()

-- postThread :: Twitter.Conduit.Manager -> Alice.Tweets.TweetThread -> IO ()
-- postThread manager (Alice.Tweets.TweetThread tweets) = go Nothing tweets
--   where
--   go _ [] = return ()
--   go parent (tweet : rest) = do
--     status <- postStatus manager parent tweet
--     let statusId = Twitter.Types.statusId status
--     go (Just statusId) rest

-- postStatus :: Twitter.Conduit.Manager -> Maybe Twitter.Types.StatusId -> Text -> IO Twitter.Types.Status
-- postStatus manager parent status = do
--   Logging.log $ "Post: " <> status
--   twInfo <- getTWInfoFromEnv
--   let
--     baseStatus = Twitter.Conduit.update status
--     statusWithParent = Lens.set Twitter.Conduit.Parameters.inReplyToStatusId parent baseStatus
--   response <- Twitter.Conduit.call twInfo manager statusWithParent
--   return response
