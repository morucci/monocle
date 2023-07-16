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
              , "metadata_updated_date" .= DateIndexMapping
              , "metadata_created_date" .= DateIndexMapping
            ]
      ]

xDocIndex :: BH.IndexName
xDocIndex = BH.IndexName "xdoc"

xMkBHEnv :: MonadIO m => m BH.BHEnv
xMkBHEnv =
  liftIO (BH.mkBHEnv <$> pure (BH.Server "http://127.0.0.1:19200") <*> Monocle.Client.mkManager)

xCreateIndex :: MonadIO m => BH.BHEnv -> m BH.Reply
xCreateIndex bhEnv = BH.runBH bhEnv $
  BH.createIndex BH.defaultIndexSettings xDocIndex

xPutMapping :: MonadIO m => BH.BHEnv -> m BH.Reply
xPutMapping bhEnv = BH.runBH bhEnv $
  BH.putMapping xDocIndex XDocIndexMapping

xWrite :: (MonadIO m, ToJSON a, XDoc a) => BH.BHEnv -> a -> m Bool
xWrite bhEnv xdoc = do
  exists <- BH.runBH bhEnv $ BH.documentExists xDocIndex (xDocGetId xdoc)
  now <- getCurrentTime
  case exists of
    False -> do
      op1 <- BH.runBH bhEnv $ BH.indexDocument xDocIndex BH.defaultIndexDocumentSettings xdoc (xDocGetId xdoc)
      void $ BH.runBH bhEnv $ BH.refreshIndex xDocIndex
      op2 <- BH.runBH bhEnv $ BH.updateDocument xDocIndex BH.defaultIndexDocumentSettings (XDocMetadataCreatedDate $ dropMilliSec now) (xDocGetId xdoc)
      op3 <- BH.runBH bhEnv $ BH.updateDocument xDocIndex BH.defaultIndexDocumentSettings (XDocMetadataUpdatedDate $ dropMilliSec now) (xDocGetId xdoc)
      trace (show op1) (pure ())
      trace (show op2) (pure ())
      trace (show op3) (pure ())
      pure $ (isSuccess op1) && (isSuccess op2) && (isSuccess op3)
    True -> do
      op1 <- BH.runBH bhEnv $ BH.updateDocument xDocIndex BH.defaultIndexDocumentSettings xdoc (xDocGetId xdoc)
      op2 <- BH.runBH bhEnv $ BH.updateDocument xDocIndex BH.defaultIndexDocumentSettings (XDocMetadataUpdatedDate $ dropMilliSec now) (xDocGetId xdoc)
      pure $ (isSuccess op1) && (isSuccess op2)

xRead :: (MonadIO m, FromJSON a, MonadCatch m) => BH.BHEnv -> BH.DocId -> m (Either BH.EsError a)
xRead bhEnv docId = do
  r <- BH.runBH bhEnv $ BH.getDocument xDocIndex docId
  BH.parseEsResponse r

class XDoc a where
  xDocGetId :: a -> DocId
  xDocRead :: (MonadIO m, MonadCatch m, FromJSON a) => DocId -> m (Maybe a)
  xDocWrite :: (MonadIO m, ToJSON a) => a -> m Bool
  xDocDelete :: MonadIO m => a -> m ()

newtype XDocLabel = XDocLabel String deriving (Show, ToJSON, FromJSON) via String
newtype XDocAuthor = XDocAuthor String deriving  (Show, ToJSON, FromJSON) via String

data XText = XText {
  xtextId :: DocId,
  xtextText :: Text,
  xtextMetadataCreatedDate :: Maybe UTCTime,
  xtextMetadataUpdatedDate :: Maybe UTCTime
} deriving (Show, Generic)

instance ToJSON XText where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON XText where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype XDocMetadataCreatedDate = XDocMetadataCreatedDate {
  xdocMetadataCreatedDate :: UTCTime
} deriving (Show, Generic)

instance ToJSON XDocMetadataCreatedDate where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON XDocMetadataCreatedDate where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype XDocMetadataUpdatedDate = XDocMetadataUpdatedDate {
  xdocMetadataUpdatedDate :: UTCTime
} deriving (Show, Generic)

instance ToJSON XDocMetadataUpdatedDate where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON XDocMetadataUpdatedDate where
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

  xDocWrite :: MonadIO m => XText -> m Bool
  xDocWrite s = do
    bhEnv <- xMkBHEnv
    xWrite bhEnv s

  xDocDelete :: MonadIO m => XText -> m ()
  xDocDelete _s = pure ()

mkXText :: Text -> Text -> XText
mkXText docRef sData = do
    XText {
      xtextId = (BH.DocId docRef),
      xtextText = sData,
      xtextMetadataCreatedDate = Nothing,
      xtextMetadataUpdatedDate = Nothing
    }
