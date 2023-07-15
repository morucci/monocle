{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}

-- | A shared library between lentilles and macroscope
module XDoc where

import Monocle.Client (mkManager)
import Monocle.Prelude

import Monocle.Backend.Index (KWMapping(..), TextAndKWMapping (TextAndKWMapping), DateIndexMapping (..))
import qualified Database.Bloodhound as BH
import Data.Aeson ( genericParseJSON, genericToJSON )
import Data.Aeson.Casing (snakeCase, aesonPrefix)
import Control.Monad.Catch
import Database.Bloodhound (isSuccess)


data XDocIndexMapping = XDocIndexMapping deriving (Eq, Show)


instance ToJSON XDocIndexMapping where
  toJSON XDocIndexMapping =
    object
      [ "properties"
          .= object
            [ "id" .= KWMapping
              , "text" .= TextAndKWMapping
              , "metadata"
                  .= object
                    [ "properties"
                        .= object
                          [ "label" .= KWMapping
                          , "updated_at" .= DateIndexMapping
                          , "created_at" .= DateIndexMapping
                          , "author" .= KWMapping
                          , "type" .= KWMapping
                          ]
                    ]
            ]
      ]

xMkBHEnv :: MonadIO m => m BH.BHEnv
xMkBHEnv =
  liftIO (BH.mkBHEnv <$> pure (BH.Server "http://127.0.0.1:19200") <*> Monocle.Client.mkManager)

xCreateIndex :: MonadIO m => BH.BHEnv -> m BH.Reply
xCreateIndex bhEnv = BH.runBH bhEnv $
  BH.createIndex BH.defaultIndexSettings (BH.IndexName "xdoc")

xPutMapping :: MonadIO m => BH.BHEnv -> m BH.Reply
xPutMapping bhEnv = BH.runBH bhEnv $
  BH.putMapping (BH.IndexName "xdoc") XDocIndexMapping

xWrite :: (MonadIO m, ToJSON xdoc) => BH.BHEnv -> DocId -> xdoc -> m Bool
xWrite bhEnv docId xdoc = do
  r <- BH.runBH bhEnv $ BH.indexDocument (BH.IndexName "xdoc") BH.defaultIndexDocumentSettings xdoc docId
  pure $ isSuccess r

xRead :: (MonadIO m, FromJSON a, MonadCatch m) => BH.BHEnv -> BH.DocId -> m (Either BH.EsError a)
xRead bhEnv docId = do
  r <- BH.runBH bhEnv $ BH.getDocument (BH.IndexName "xdoc") docId
  BH.parseEsResponse r

class XDoc a where
  xDocGetId :: a -> DocId
  xDocRead :: (MonadIO m, MonadCatch m, FromJSON a) => DocId -> m (Maybe a)
  xDocWrite :: (MonadIO m, ToJSON a) => a -> m Bool
  xDocDelete :: MonadIO m => a -> m ()

newtype XDocLabel = XDocLabel String deriving (Show, ToJSON, FromJSON) via String
newtype XDocAuthor = XDocAuthor String deriving  (Show, ToJSON, FromJSON) via String

data XText = XString {
  xtextId :: DocId,
  xtextText :: Text,
  xtextMetadata :: XDocMetadata
} deriving (Show, Generic)

instance ToJSON XText where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON XText where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data XDocMetadata = XDocMetadata {
  xdocmetadataLabel :: XDocLabel,
  xdocmetadataUpdatedDate :: UTCTime,
  xdocmetadataCreatedDate :: UTCTime,
  xdocmetadataAuthor :: XDocAuthor
} deriving (Show, Generic)

instance ToJSON XDocMetadata where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON XDocMetadata where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance XDoc XText where
  xDocGetId :: XText -> DocId
  xDocGetId s = xtextId s

  xDocRead :: (MonadIO m, MonadCatch m) => DocId -> m (Maybe XText)
  xDocRead docId = do
    bhEnv <- xMkBHEnv
    rE <- xRead bhEnv docId
    case rE of
      Left _  -> pure Nothing
      Right xText -> pure . getHit $ BH.foundResult xText
    where
      getHit (Just (BH.EsResultFound _ cm)) = Just cm
      getHit Nothing = Nothing

  xDocWrite :: MonadIO m =>  XText -> m Bool
  xDocWrite  s = do
    bhEnv <- xMkBHEnv
    xWrite bhEnv (xDocGetId s) s

  xDocDelete :: MonadIO m => XText -> m ()
  xDocDelete _s = pure ()

mkXString :: Text -> Text -> IO XText
mkXString docRef sData = do
    now <- getCurrentTime
    pure $ XString {
      xtextId = (BH.DocId docRef),
      xtextText = sData,
      xtextMetadata = XDocMetadata {
          xdocmetadataLabel = XDocLabel "MyLabel",
          xdocmetadataUpdatedDate = now,
          xdocmetadataCreatedDate = now,
          xdocmetadataAuthor = XDocAuthor "AnAuthor"
        }
      }
