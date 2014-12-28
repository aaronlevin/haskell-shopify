{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Types.MetafieldType (
  MetafieldType(..)
  ) where

import           Control.Monad (mzero)
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))

data MetafieldType = MetafieldString
                   | MetafieldInteger
                   deriving (Eq, Ord, Show)

instance FromJSON MetafieldType where
  parseJSON(A.String "string")  = return MetafieldString
  parseJSON(A.String "integer") = return MetafieldInteger
  parseJSON _                   = mzero

instance ToJSON MetafieldType where
  toJSON MetafieldString  = A.String "string"
  toJSON MetafieldInteger = A.String "integer"
