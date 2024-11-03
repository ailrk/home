{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Aws qualified
import Aws.Core (Protocol(HTTPS))
import Aws.S3 (ObjectInfo (..))
import Aws.S3 qualified as S3
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Conduit ( runResourceT, MonadThrow )
import Text.URI (URI)
import Text.URI.Lens
import Text.URI qualified as URI
import System.Environment (getEnv)
import Lucid
import Lens.Micro


data Book = Book
  { key :: Text
  , link :: URI
  }
  deriving (Show)


toBook :: MonadThrow m => URI -> ObjectInfo -> m Book
toBook uri ObjectInfo {..} = do
  pathPiece <- URI.mkPathPiece objectKey
  pure Book
    { key = objectKey
    , link = uri & uriPath %~ (<> [pathPiece])
    }


main :: IO ()
main = do
  bucketName <- Text.pack <$> getEnv "R2_BUCKET_NAME"
  url <- getEnv "URL" >>= URI.mkURI . Text.pack
  endpoint <- do
    uri <- getEnv "R2_JURISDICTION_SPECIFIC_ENDPOINTS" >>= URI.mkURI . Text.pack
    case Text.encodeUtf8 . URI.unRText . URI.authHost <$> URI.uriAuthority uri of
      Left _ -> fail "No"
      Right e -> pure e

  cfg <- Aws.baseConfiguration
  let scfg = (S3.s3v4 HTTPS endpoint False S3.SignWithEffort)
        { S3.s3Region = Just "auto"
        }

  mgr <- newManager tlsManagerSettings
  objInfos <- runResourceT $ do
    S3.GetBucketResponse { S3.gbrContents = rsp } <- Aws.pureAws cfg scfg mgr $ do
      S3.getBucket bucketName
    pure rsp

  books <- traverse (toBook url) objInfos
  renderToFile "Books.html" $ do
    ul_ $ mconcat $ fmap mklink books


mklink :: Book -> Html ()
mklink Book {..} = do
  li_ $ a_ [href_ (URI.render link)] (toHtml key)
