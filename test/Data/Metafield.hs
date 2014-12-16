module Test.Data.Metafield where

import Test.QuickCheck
import Test.Tasty.HUnit
import Text.RawString.QQ
import Data.Either


-- /admin/customers/342305289/metafields.json
metafields :: String
metafields = [r|
{  
   "metafields":[  
      {  
         "created_at":"2014-12-15T21:00:27-05:00",
         "description":null,
         "id":1475823249,
         "key":"Bar",
         "namespace":"Bar",
         "owner_id":342305289,
         "updated_at":"2014-12-15T21:00:27-05:00",
         "value":123,
         "value_type":"integer",
         "owner_resource":"customer"
      },
      {  
         "created_at":"2014-12-15T21:00:20-05:00",
         "description":null,
         "id":1475823149,
         "key":"Foo",
         "namespace":"Foo",
         "owner_id":342305289,
         "updated_at":"2014-12-15T21:00:20-05:00",
         "value":"Foo value",
         "value_type":"string",
         "owner_resource":"customer"
      }
   ]
}
|]

-- /admin/metafields/1475823249.json
metafield :: String
metafield = [r|
{  
   "metafield":{  
      "created_at":"2014-12-15T21:00:27-05:00",
      "description":null,
      "id":1475823249,
      "key":"Bar",
      "namespace":"Bar",
      "owner_id":342305289,
      "updated_at":"2014-12-15T21:00:27-05:00",
      "value":123,
      "value_type":"integer",
      "owner_resource":"customer"
   }
}
|]
