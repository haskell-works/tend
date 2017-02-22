{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Map
import Data.Text.Lazy
import Dhall
import Lib
import Network.Wreq
import qualified Network.Wreq as W
import System.Directory
import qualified Data.Text as ST
import Git
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Git.Libgit2 (MonadLg, LgRepo, lgFactory)
import System.Process
import System.IO
import Prelude hiding (lines)
import qualified Prelude as P
import qualified Data.List.Extra as LE

data CircleConfig = CircleConfig
  { apiToken :: Text
  } deriving (Generic, Show)

makeLenses ''CircleConfig

instance Interpret CircleConfig

splitByChar :: Char -> String -> [String]
splitByChar c s = case rest of
  []     -> [chunk]
  _:rest -> chunk : splitByChar c rest
  where (chunk, rest) = P.break (==c) s

getMe :: W.Options -> IO ()
getMe opts = do
  r <- getWith opts "https://circleci.com/api/v1.1/me"
  print r
  print (r ^. responseStatus)
  print (r ^. responseStatus . statusCode)

setEnv :: W.Options -> IO ()
setEnv opts = do
  let vars = fromList
        [ ("name", "foo")
        , ("value", "bar")
        ] :: Map String String
  r <- postWith opts "https://circleci.com/api/v1.1/project/github/pico-works/pico-disposal/envvar" (toJSON vars)
  print r

data GithubRemote = GithubRemote
  { githubRemoteOrganisation :: String
  , githubRemoteProject      :: String
  } deriving (Eq, Show)

remoteEntriesFromLine :: String -> Maybe (String, GithubRemote)
remoteEntriesFromLine s = case LE.split (== ' ') (LE.replace "\t" " " s) of
  alias : uri : _ : xs -> case LE.split (== ':') uri of
    [userHost, uriPath] -> if userHost == "git@github.com"
      then case LE.split (== '/') uriPath of
        [organisation, gitFile] -> case LE.split (== '.') gitFile of
          [project, "git"] -> Just (alias, GithubRemote organisation project)
          _ -> Nothing
        _ -> Nothing
      else Nothing
  _ -> Nothing

main :: IO ()
main = do
  remoteLines <- P.lines <$> readProcess "git" ["remote", "-v"] ""
  let remoteEntries = LE.nub (remoteEntriesFromLine <$> remoteLines)
  print remoteEntries

  let repoOpts = RepositoryOptions
        { repoPath = "." :: String
        , repoWorkingDir = Nothing
        , repoIsBare = False
        , repoAutoCreate = False
        }
  repo <- openRepository lgFactory defaultRepositoryOptions { repoPath = "." }

  withRepository lgFactory "." $ do
    maybeRef <- lookupReference "HEAD"
    case maybeRef of
      Just ref -> do
        oid <- referenceToOid ref
        liftIO $ print "==="
        liftIO $ print oid
        liftIO $ print "==="
      Nothing -> liftIO $ putStrLn "No ref"
    return ()

  home <- pack <$> getHomeDirectory
  circleConfig <- input auto (home `append` "/.circle/config")
  print (circleConfig :: CircleConfig)
  let apiToken' = toStrict (apiToken circleConfig)
  let opts = defaults & param "circle-token" .~ [apiToken']
  getMe opts

  return ()
