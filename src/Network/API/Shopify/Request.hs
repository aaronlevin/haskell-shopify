{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Request (
  ) where

import Data.Default (def)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Shopify.Types (ProductId, MetafieldId, VariantId)
import Network.HTTP.Client.Conduit (method, Request(host, path))
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

-- | default request
defaultRequest :: Request
defaultRequest = def

-- | base url
shopifyHost :: String
shopifyHost = "api.shopify.com"

-- | base product urls
createProductUrl :: Request
createProductUrl = defaultRequest { method = methodPost
                                  , host = encodeUtf8 . pack $ shopifyHost
                                  , path = encodeUtf8 . pack $ "/admin/products.json"
                                  }
readProductUrl :: ProductId -> Request
readProductUrl (ProductId i) = defaultRequest { method = methodGet
                                              , host = encodeUtf8 . pack $ shopifyHost
                                              ,  path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                              }
updateProductUrl :: ProductId -> Request
updateProductUrl (ProductId i) = defaultRequest { method = methodPut
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                                }
deleteProductUrl :: ProductId -> Request
deleteProductUrl (ProductId i) = defaultRequest { method = methodDelete
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                                }

-- | base metafield urls
createMetafieldUrl :: Request
createMetafieldUrl = defaultRequest { method = methodPost
                                    , host = encodeUtf8 . pack $ shopifyHost
                                    , path = encodeUtf8 . pack $ "/admin/metafields.json"
                                    }
readMetafieldUrl :: MetafieldId -> Request
readMetafieldUrl (MetafieldId i) = defaultRequest { method = methodGet
                                                  , host = encodeUtf8 . pack $ shopifyHost
                                                  , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                  }
updateMetafieldUrl :: MetafieldId -> Request
updateMetafieldUrl (MetafieldId i) = defaultRequest { method = methodPut
                                                    , host = encodeUtf8 . pack $ shopifyHost
                                                    , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                    }
deleteMetafieldUrl :: MetafieldId -> Request
deleteMetafieldUrl (MetafieldId i) = defaultRequest { method = methodDelete
                                                    , host = encodeUtf8 . pack $ shopifyHost
                                                    , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                    }

-- | base variant urls
createVariantUrl :: Request
createVariantUrl = defaultRequest { method = methodPost
                                  , host = encodeUtf8 . pack $ shopifyHost
                                  , path = encodeUtf8 . pack $ "/admin/variants.json"
                                  }
readVariantUrl :: VariantId -> Request
readVariantUrl (VariantId i) = defaultRequest { method = methodGet
                                              , host = encodeUtf8 . pack $ shopifyHost
                                              , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                              }
updateVariantUrl :: VariantId -> Request
updateVariantUrl (VariantId i) = defaultRequest { method = methodPut
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                                }
deleteVariantUrl :: VariantId -> Request
deleteVariantUrl (VariantId i) = defaultRequest { method = methodDelete
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                                }

