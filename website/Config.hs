module Config
  ( staticDir
  , root
  , config
  , myEmail
  , myAvatar
  , myGithub
  ) where

import Data.Text (Text, unpack, pack, isPrefixOf)
import Text.URI.QQ
import Text.URI
import System.FilePath.Posix (takeFileName)
import Hakyll (Configuration (..), defaultConfiguration)


staticDir :: Text
staticDir = "static/"


root :: URI
root = [uri|https://www.ailrk.com|]


config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "build/site"
  , storeDirectory = "build/store"
  , tmpDirectory = "build/tmp"
  , providerDirectory = unpack staticDir
  , ignoreFile = ignore
  }
 where
   ignore path = "." `isPrefixOf` pack (takeFileName path)



myEmail :: Text
myEmail = "jimmy123good@gmail.com"


myAvatar :: Text
myAvatar = ""


myGithub :: Text
myGithub = "ailrk"
