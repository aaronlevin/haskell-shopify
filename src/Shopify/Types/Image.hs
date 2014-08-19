{-# LANGUAGE OverloadedStrings #-}

module Shopify.Types.Image(
  Image(..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Aeson ((.:), FromJSON(..), Value(Object))
import Data.Text (Text)

data Image = Image { imageSrc :: Text }

instance FromJSON Image where
  parseJSON (Object v) = Image <$> v .: "src"
  parseJSON _          = mzero
