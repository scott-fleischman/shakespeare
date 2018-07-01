{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

import qualified Shakespeare.Twitter
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import qualified Control.Logging as Logging
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import           Data.Generics.Product (field, typed)
import qualified Data.Maybe as Maybe
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import           GHC.Generics (Generic)
import           Formatting ((%))
import qualified Formatting
import qualified Network.HTTP.Client as HTTP.Client
import qualified Web.Twitter.Conduit as Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as Twitter.Conduit.Parameters
import qualified Web.Twitter.Types as Twitter.Types

data Env = Env
  { envNameToTWInfo :: Map Text Twitter.Conduit.TWInfo
  , envTweetDelayMilliseconds :: Int
  , envManager :: HTTP.Client.Manager
  } deriving Generic

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

  manager <- Twitter.Conduit.newManager Twitter.Conduit.tlsManagerSettings
  let env = Env nameToTWInfo (config ^. field @"tweetDelayMilliseconds") manager

  Monad.forM_ (book ^. field @"threads") $ \thread -> do
    tryPostThread env thread
    makeDelay env

makeDelay :: Env -> IO ()
makeDelay env = Concurrent.threadDelay (envTweetDelayMilliseconds env * 1000)

tryPostThread :: Env -> Shakespeare.Twitter.Thread -> IO ()
tryPostThread env thread = do
  possibleResult <- Exception.try @Exception.SomeException (postThread env thread)
  case possibleResult of
    Left err -> Logging.warn $ Formatting.sformat Formatting.shown err
    Right () -> return ()

postThread :: Env -> Shakespeare.Twitter.Thread -> IO ()
postThread env (Shakespeare.Twitter.Thread tweets) = go Nothing tweets
  where
  go _ [] = return ()
  go parent (tweet : rest) = do
    status <- postStatus env parent tweet
    let statusId = Twitter.Types.statusId status
    go (Just statusId) rest

postStatus :: Env -> Maybe Twitter.Types.StatusId -> Shakespeare.Twitter.Tweet -> IO Twitter.Types.Status
postStatus env parent tweet = do
  let
    authorName = tweet ^. field @"author"
    status = tweet ^. field @"text"
  twInfo <-
    case Map.lookup authorName (envNameToTWInfo env) of
      Nothing -> do
        Logging.log $ Formatting.sformat ("Missing author: " % Formatting.stext) authorName
        fail "Missing author"
      Just x -> return x
  Logging.log $ Formatting.sformat ("Tweet: " % Formatting.stext % ": " % Formatting.stext) authorName status
  let
    baseStatus = Twitter.Conduit.update status
    statusWithParent = Lens.set Twitter.Conduit.Parameters.inReplyToStatusId parent baseStatus
  response <- Twitter.Conduit.call twInfo (envManager env) statusWithParent
  makeDelay env
  return response
