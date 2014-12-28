{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Types.Option(
  Option(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson ((.:), (.=), FromJSON(parseJSON), object, ToJSON(toJSON), Value(Object))

import Data.Text (Text)

data Option = Option { optionId        :: Integer
                     , optionName      :: Text
                     , optionPosition  :: Int
                     , optionProductId :: Integer
                     }
                     deriving (Eq, Ord, Show)

instance FromJSON Option where
  parseJSON (Object v) = Option <$> v .: "id"
                                <*> v .: "name"
                                <*> v .: "position"
                                <*> v .: "product_id"
  parseJSON _          = mzero

instance ToJSON Option where
  toJSON (Option oid name position prod) = object [ "oid"        .= oid
                                                  , "name"       .= name
                                                  , "position"   .= position
                                                  , "product_id" .= prod
                                                  ]
