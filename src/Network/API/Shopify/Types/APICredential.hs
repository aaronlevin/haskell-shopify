module Network.API.Shopify.Types.APICredential(
  APICredential(..)
  ) where

import Data.Text (Text)

-- | Simpel data type encompassing the two ways one can authorize an api call.
-- either OAuth tokens or api_secret and password for private apps.
data APICredential = OAuthCred Text
                   | BasicCred Text Text
                   -- ^ BasicCred `apiKey` `password`
