{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}

-- | A shared library between lentilles and macroscope
module XDoc where

import Monocle.Client (mkManager)
import Monocle.Prelude

import Control.Monad.Catch
import Data.Aeson (genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Database.Bloodhound (isSuccess)
import Database.Bloodhound qualified as BH
import Monocle.Backend.Index (DateIndexMapping (..), KWMapping (..), TextAndKWMapping (TextAndKWMapping))

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
xCreateIndex bhEnv =
  BH.runBH bhEnv $
    BH.createIndex BH.defaultIndexSettings xDocIndex

xPutMapping :: MonadIO m => BH.BHEnv -> m BH.Reply
xPutMapping bhEnv =
  BH.runBH bhEnv $
    BH.putMapping xDocIndex XDocIndexMapping

xCreate :: (MonadIO m, ToJSON a, XDoc a) => BH.BHEnv -> a -> m Bool
xCreate bhEnv xdoc = do
  op1 <- BH.runBH bhEnv $ BH.indexDocument xDocIndex BH.defaultIndexDocumentSettings xdoc (xDocGetId xdoc)
  void $ BH.runBH bhEnv $ BH.refreshIndex xDocIndex
  pure $ isSuccess op1

xExists :: MonadIO m => BH.BHEnv -> DocId -> m Bool
xExists bhEnv docId = BH.runBH bhEnv $ BH.documentExists xDocIndex docId

xUpdate :: (MonadIO m, ToJSON a, XDoc a) => BH.BHEnv -> a -> m Bool
xUpdate bhEnv xdoc = do
  op1 <- BH.runBH bhEnv $ BH.updateDocument xDocIndex BH.defaultIndexDocumentSettings xdoc (xDocGetId xdoc)
  pure $ isSuccess op1

xRead :: (MonadIO m, FromJSON a, MonadCatch m) => BH.BHEnv -> BH.DocId -> m (Either BH.EsError a)
xRead bhEnv docId = do
  exists <- xExists bhEnv docId
  case exists of
    True -> do
      r <- BH.runBH bhEnv $ BH.getDocument xDocIndex docId
      BH.parseEsResponse r
    False -> do
      pure $ Left $ BH.EsError 1 ""

class XDoc a where
  xDocGetId :: a -> DocId
  xDocRead :: (MonadIO m, MonadCatch m, FromJSON a) => DocId -> m (Maybe a)
  xDocCreate :: (MonadIO m, ToJSON a) => a -> m Bool
  xDocUpdate :: (MonadIO m, ToJSON a) => a -> m Bool
  xDocDelete :: MonadIO m => a -> m ()

newtype XDocLabel = XDocLabel String deriving (Show, ToJSON, FromJSON) via String
newtype XDocAuthor = XDocAuthor String deriving (Show, ToJSON, FromJSON) via String

data XText = XText
  { xtextId :: DocId
  , xtextText :: Text
  , xtextMetadataCreatedDate :: UTCTime
  , xtextMetadataUpdatedDate :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON XText where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON XText where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance XDoc XText where
  xDocGetId :: XText -> DocId
  xDocGetId s = xtextId s

  xDocRead :: (MonadIO m, MonadCatch m) => DocId -> m (Maybe XText)
  xDocRead docId = do
    bhEnv <- xMkBHEnv
    rE <- xRead bhEnv docId
    case rE of
      Left _ -> pure Nothing
      Right xText -> pure . getHit $ BH.foundResult xText
   where
    getHit (Just (BH.EsResultFound _ cm)) = Just cm
    getHit Nothing = Nothing

  xDocCreate :: MonadIO m => XText -> m Bool
  xDocCreate s = do
    bhEnv <- xMkBHEnv
    exists <- xExists bhEnv (xDocGetId s)
    case exists of
      True -> pure False
      False -> xCreate bhEnv s

  xDocUpdate :: MonadIO m => XText -> m Bool
  xDocUpdate s = do
    bhEnv <- xMkBHEnv
    exists <- xExists bhEnv (xDocGetId s)
    case exists of
      True -> xUpdate bhEnv s
      False -> pure False

  xDocDelete :: MonadIO m => XText -> m ()
  xDocDelete _s = pure ()

setXText :: forall m. (MonadIO m, MonadCatch m) => Text -> Text -> m (Bool, XText)
setXText docRef sData = do
  now <- dropMilliSec <$> getCurrentTime
  current_docM <- xDocRead (BH.DocId docRef) :: m (Maybe XText)
  case current_docM of
    Just current_doc -> do
      let doc =
            current_doc
              { xtextText = sData
              , xtextMetadataUpdatedDate = now
              }
      updated <- xDocUpdate doc
      pure (updated, doc)
    Nothing -> do
      let doc =
            XText
              { xtextId = (BH.DocId docRef)
              , xtextText = sData
              , xtextMetadataCreatedDate = now
              , xtextMetadataUpdatedDate = now
              }
      created <- xDocCreate doc
      pure (created, doc)

getXText :: forall m. (MonadIO m, MonadCatch m) => Text -> m (Maybe XText)
getXText docRef = do
  xDocRead (BH.DocId docRef) :: m (Maybe XText)
