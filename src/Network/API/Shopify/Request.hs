{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Request where

import Data.Default (def)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Shopify.Types (ProductId(ProductId), MetafieldId(MetafieldId), VariantId(VariantId))
import Network.HTTP.Client.Conduit (method, Request(host, path))
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

-- | default request
defaultRequest :: Request
defaultRequest = def

-- | base url
shopifyHost :: String
shopifyHost = "api.shopify.com"

-- | base product urls
createProductReq :: Request
createProductReq = defaultRequest { method = methodPost
                                  , host = encodeUtf8 . pack $ shopifyHost
                                  , path = encodeUtf8 . pack $ "/admin/products.json"
                                  }
readProductReq :: ProductId -> Request
readProductReq (ProductId i) = defaultRequest { method = methodGet
                                              , host = encodeUtf8 . pack $ shopifyHost
                                              ,  path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                              }
readProductsReq :: Request
readProductsReq = defaultRequest { method = methodGet
                                 , host = encodeUtf8 . pack $ shopifyHost
                                 , path = encodeUtf8 . pack $ "/admin/products.json"
                                 }

updateProductReq :: ProductId -> Request
updateProductReq (ProductId i) = defaultRequest { method = methodPut
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                                }
deleteProductReq :: ProductId -> Request
deleteProductReq (ProductId i) = defaultRequest { method = methodDelete
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                                }

-- | base metafield urls
createMetafieldReq :: Request
createMetafieldReq = defaultRequest { method = methodPost
                                    , host = encodeUtf8 . pack $ shopifyHost
                                    , path = encodeUtf8 . pack $ "/admin/metafields.json"
                                    }
readMetafieldReq :: MetafieldId -> Request
readMetafieldReq (MetafieldId i) = defaultRequest { method = methodGet
                                                  , host = encodeUtf8 . pack $ shopifyHost
                                                  , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                  }
updateMetafieldReq :: MetafieldId -> Request
updateMetafieldReq (MetafieldId i) = defaultRequest { method = methodPut
                                                    , host = encodeUtf8 . pack $ shopifyHost
                                                    , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                    }
deleteMetafieldReq :: MetafieldId -> Request
deleteMetafieldReq (MetafieldId i) = defaultRequest { method = methodDelete
                                                    , host = encodeUtf8 . pack $ shopifyHost
                                                    , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                    }

-- | base variant urls
createVariantReq :: Request
createVariantReq = defaultRequest { method = methodPost
                                  , host = encodeUtf8 . pack $ shopifyHost
                                  , path = encodeUtf8 . pack $ "/admin/variants.json"
                                  }
readVariantReq :: VariantId -> Request
readVariantReq (VariantId i) = defaultRequest { method = methodGet
                                              , host = encodeUtf8 . pack $ shopifyHost
                                              , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                              }
updateVariantReq :: VariantId -> Request
updateVariantReq (VariantId i) = defaultRequest { method = methodPut
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                                }
deleteVariantReq :: VariantId -> Request
deleteVariantReq (VariantId i) = defaultRequest { method = methodDelete
                                                , host = encodeUtf8 . pack $ shopifyHost
                                                , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                                }

