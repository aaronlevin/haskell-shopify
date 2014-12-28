{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Types.Image(
  Image(..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Aeson ((.:), (.=), FromJSON(parseJSON), ToJSON(toJSON), object, Value(Object))
import Data.Text (Text)

data Image = Image { imageSrc :: Text } deriving (Eq, Ord, Show)

instance FromJSON Image where
  parseJSON (Object v) = Image <$> v .: "src"
  parseJSON _          = mzero

instance ToJSON Image where
  toJSON (Image imgSrc) = object ["src" .= imgSrc]
