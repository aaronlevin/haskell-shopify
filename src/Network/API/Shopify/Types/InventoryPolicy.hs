{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Types.InventoryPolicy(
  InventoryPolicy(..)
  ) where

import           Control.Monad (mzero)
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))

data InventoryPolicy = InventoryDeny
                       -- ^ (default) Customers are not allowed to place orders
                       --   for a product variant when it's out of stock.
                     | InventoryContinue
                       -- ^ Customers are allowed to place orders for a product
                       --   variant when it's out of stock.
                     deriving (Eq, Ord, Show)

instance FromJSON InventoryPolicy where
  parseJSON(A.String "deny")     = return InventoryDeny
  parseJSON(A.String "continue") = return InventoryContinue
  parseJSON _                    = mzero

instance ToJSON InventoryPolicy where
  toJSON InventoryDeny     = A.String "deny"
  toJSON InventoryContinue = A.String "continue"
