module Network.API.Shopify.Types.StoreName(
    StoreName(..)
  ) where

import Data.Text (Text)

-- | newtype wrapper around a store's name.
newtype StoreName = StoreName Text
