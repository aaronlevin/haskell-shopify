{-# LANGUAGE OverloadedStrings #-}

module Network.API.Shopify.Request where

import Data.Default (def)
import Data.Text (append, pack, Text)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Shopify.Types (MetafieldId(MetafieldId), APICredential(OAuthCred, BasicCred), ProductId(ProductId), StoreName(StoreName), VariantId(VariantId))
import Network.HTTP.Client.Conduit (applyBasicAuth, method, Request(host, path, requestHeaders))
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

-- | default request
defaultRequest :: Request
defaultRequest = def

-- | base url
shopifyHost :: StoreName -> Text
shopifyHost (StoreName storeName) = storeName `append` ".shopify.com"

-- | base product urls
createProductReq :: StoreName -> Request
createProductReq storeName = defaultRequest { method = methodPost
                                  , host = encodeUtf8 $ shopifyHost storeName
                                  , path = encodeUtf8 . pack $ "/admin/products.json"
                                  }
readProductReq :: StoreName -> ProductId -> Request
readProductReq storeName (ProductId i) = defaultRequest { method = methodGet
                                              , host = encodeUtf8 $ shopifyHost storeName
                                              ,  path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                              }
readProductsReq :: StoreName -> Request
readProductsReq storeName = defaultRequest { method = methodGet
                                 , host = encodeUtf8 $ shopifyHost storeName
                                 , path = encodeUtf8 . pack $ "/admin/products.json"
                                 }

updateProductReq :: StoreName -> ProductId -> Request
updateProductReq storeName (ProductId i) = defaultRequest { method = methodPut
                                                , host = encodeUtf8 $ shopifyHost storeName
                                                , path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                                }
deleteProductReq :: StoreName -> ProductId -> Request
deleteProductReq storeName (ProductId i) = defaultRequest { method = methodDelete
                                                , host = encodeUtf8 $ shopifyHost storeName
                                                , path = encodeUtf8 . pack $ "/admin/products/" ++ show i ++ ".json"
                                                }

-- | base metafield urls
createMetafieldReq :: StoreName -> Request
createMetafieldReq storeName = defaultRequest { method = methodPost
                                    , host = encodeUtf8 $ shopifyHost storeName
                                    , path = encodeUtf8 . pack $ "/admin/metafields.json"
                                    }
readMetafieldReq :: StoreName -> MetafieldId -> Request
readMetafieldReq storeName (MetafieldId i) = defaultRequest { method = methodGet
                                                  , host = encodeUtf8 $ shopifyHost storeName
                                                  , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                  }
updateMetafieldReq :: StoreName -> MetafieldId -> Request
updateMetafieldReq storeName (MetafieldId i) = defaultRequest { method = methodPut
                                                    , host = encodeUtf8 $ shopifyHost storeName
                                                    , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                    }
deleteMetafieldReq :: StoreName -> MetafieldId -> Request
deleteMetafieldReq storeName (MetafieldId i) = defaultRequest { method = methodDelete
                                                    , host = encodeUtf8 $ shopifyHost storeName
                                                    , path = encodeUtf8 . pack $ "/admin/metafields/" ++ show i ++ ".json"
                                                    }

-- | base variant urls
createVariantReq :: StoreName -> Request
createVariantReq storeName = defaultRequest { method = methodPost
                                  , host = encodeUtf8 $ shopifyHost storeName
                                  , path = encodeUtf8 . pack $ "/admin/variants.json"
                                  }
readVariantReq :: StoreName -> VariantId -> Request
readVariantReq storeName (VariantId i) = defaultRequest { method = methodGet
                                              , host = encodeUtf8 $ shopifyHost storeName
                                              , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                              }
updateVariantReq :: StoreName -> VariantId -> Request
updateVariantReq storeName (VariantId i) = defaultRequest { method = methodPut
                                                , host = encodeUtf8 $ shopifyHost storeName
                                                , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                                }
deleteVariantReq :: StoreName -> VariantId -> Request
deleteVariantReq storeName (VariantId i) = defaultRequest { method = methodDelete
                                                , host = encodeUtf8 $ shopifyHost storeName
                                                , path = encodeUtf8 . pack $ "/admin/variants/" ++ show i ++ ".json"
                                                }

-- | simple method to add the access token header to a request.
authorizeRequest :: APICredential -> Request -> Request
authorizeRequest (OAuthCred token) req = req { requestHeaders = newHeaders }
    where newHeaders = ("X-Shopify-Access-Token", encodeUtf8 token) : requestHeaders req
authorizeRequest (BasicCred apiKey password) req = applyBasicAuth apiBytes passBytes req
    where apiBytes  = encodeUtf8 apiKey
          passBytes = encodeUtf8 password

