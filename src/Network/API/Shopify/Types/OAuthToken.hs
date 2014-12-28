module Network.API.Shopify.Types.OAuthToken(
  OAuthToken(..)
  ) where

import Data.Text (Text)

newtype OAuthToken = OAuthToken Text
