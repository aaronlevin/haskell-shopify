module Shopify.Types.Option(
  Option(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

newtype Option = Option { runOption :: Map Text Text }
