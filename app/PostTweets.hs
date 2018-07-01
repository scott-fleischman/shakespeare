{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

import qualified Shakespeare.Twitter
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception.Safe as Exception.Safe
import           Control.Lens ((^.), (^?))
import qualified Control.Lens as Lens
import qualified Control.Logging as Logging
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import           Data.Generics.Product (field, typed)
import           Data.Generics.Sum (_Ctor)
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

data UserInfo = UserInfo
  { userInfoHandle :: Text
  , userInfoTWInfo :: Twitter.Conduit.TWInfo
  } deriving Generic

data Env = Env
  { envNameToTWInfo :: Map Text UserInfo
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

    makeCred :: Shakespeare.Twitter.UserInfo -> Twitter.Conduit.Credential
    makeCred tokenInfo =
      Twitter.Conduit.Credential
      [ ("oauth_token",         Text.Encoding.encodeUtf8 $ tokenInfo ^. field @"token")
      , ("oauth_token_secret",  Text.Encoding.encodeUtf8 $ tokenInfo ^. field @"secret")
      ]

    makeUserInfo userInfo
      = (\cred ->
        UserInfo
          (userInfo ^. field @"handle")
          (Twitter.Conduit.setCredential oauth cred Twitter.Conduit.def))
      $ makeCred userInfo
    nameToUserInfo = fmap makeUserInfo $ config ^. field @"accounts"

  Logging.log "Loading book.json"
  book <- Aeson.eitherDecodeFileStrict @Shakespeare.Twitter.Book "book.json" >>= 
    \case
      Left err -> error err
      Right result -> return result

  let
    allAuthors = Set.fromList $ Lens.toListOf (field @"threads" . traverse . field @"tweets" . traverse . field @"author") book
    authorMismatches = Set.filter (flip Map.notMember nameToUserInfo) allAuthors
  if Set.null authorMismatches
    then return ()
    else do
      Logging.log "Missing names in config:"
      mapM_ Logging.log $ Set.toList authorMismatches
      error "Missing names in config"

  manager <- Twitter.Conduit.newManager Twitter.Conduit.tlsManagerSettings
  let env = Env nameToUserInfo (config ^. field @"tweetDelayMilliseconds") manager

  Monad.forM_ (book ^. field @"threads") $ \thread -> do
    tryPostThread env thread
    makeDelay env

makeDelay :: Env -> IO ()
makeDelay env = Concurrent.threadDelay (envTweetDelayMilliseconds env * 1000)

tryPostThread :: Env -> Shakespeare.Twitter.Thread -> IO ()
tryPostThread env thread = do
  possibleResult <- Exception.Safe.tryAny (postThread env thread)
  case possibleResult of
    Left err -> Logging.warn $ Formatting.sformat Formatting.shown err
    Right () -> return ()

postThread :: Env -> Shakespeare.Twitter.Thread -> IO ()
postThread env (Shakespeare.Twitter.Thread tweets) = go Nothing tweets
  where
  go _ [] = return ()
  go parent (tweet : rest) = do
    (status, handle) <- postStatus env parent tweet

    case rest of
      [] -> return ()
      (_ : _) -> makeDelay env

    let statusId = Twitter.Types.statusId status
    go (Just (statusId, handle)) rest

postStatus :: Env -> Maybe (Twitter.Types.StatusId, Text) -> Shakespeare.Twitter.Tweet -> IO (Twitter.Types.Status, Text)
postStatus env maybeParentInfo tweet = do
  let
    authorName = tweet ^. field @"author"
    rawStatus = tweet ^. field @"text"
    maybeStatusId = maybeParentInfo ^? _Ctor @"Just" . typed @Twitter.Types.StatusId
  userInfo <-
    case Map.lookup authorName (envNameToTWInfo env) of
      Nothing -> do
        Logging.log $ Formatting.sformat ("Missing author: " % Formatting.stext) authorName
        fail "Missing author"
      Just x -> return x
  let
    userHandle = userInfoHandle userInfo
    status =
      case maybeParentInfo of
        Just (_, parentHandle) | parentHandle /= userHandle -> Text.concat ["@", parentHandle, " ", rawStatus]
        _ -> rawStatus
  Logging.log $ Formatting.sformat ("Tweet: " % Formatting.stext % ": " % Formatting.stext) authorName status
  let
    baseStatus = Twitter.Conduit.update status
    statusWithParent = Lens.set Twitter.Conduit.Parameters.inReplyToStatusId maybeStatusId baseStatus
    twInfo = userInfoTWInfo userInfo
  response <- Twitter.Conduit.call twInfo (envManager env) statusWithParent
  return (response, userHandle)
