{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Types.Metafield (
  Metafield(..)
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson ( (.:)
                            , (.:?)
                            , (.=)
                            , FromJSON(parseJSON)
                            , object
                            , Value(Object)
                            , ToJSON(toJSON)
                            )
import qualified Data.Aeson as A
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Network.API.Shopify.Types.MetafieldType (MetafieldType)

data Metafield = Metafield { metafieldCreatedAt :: UTCTime
                           , metafieldDescription :: Maybe Text
                           , metafieldId :: Integer
                           , metafieldKey :: Text
                           , metafieldNamespace :: Text
                           , metafieldOwnerId :: Integer
                           , metafieldOwnerResources :: Text
                           , metafieldValue :: Text
                           , metafieldType :: MetafieldType
                           , metafieldUpdatedAt :: UTCTime
                           }

instance FromJSON Metafield where
  parseJSON(Object v) = Metafield <$> v .:  "created_at"
                                  <*> v .:? "description"
                                  <*> v .:  "id"
                                  <*> v .:  "key"
                                  <*> v .:  "namespace"
                                  <*> v .:  "owner_id"
                                  <*> v .:  "owner_resource"
                                  <*> v .:  "value"
                                  <*> v .:  "value_type"
                                  <*> v .:  "updated_at"
  parseJSON _         = mzero

instance ToJSON Metafield where
  toJSON (Metafield createdAt
                    description
                    metaId
                    key
                    namespace
                    ownerId
                    ownerResource
                    value
                    valueType
                    updatedAt
                    ) = object $ [ "created_at"    .= createdAt 
                                 , "id"            .= metaId
                                 , "key"           .= key
                                 , "namespace"     .= namespace
                                 , "owner_id"      .= ownerId
                                 , "ownerResource" .= ownerResource
                                 , "value"         .= value
                                 , "value_type"    .= valueType
                                 , "updated_at"    .= updatedAt
                                 ] ++ catMaybes
                                 [ ("description" .=) . A.String <$> description ]
