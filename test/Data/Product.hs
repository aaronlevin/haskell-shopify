module Test.Data.Product where

import Test.QuickCheck
import Test.Tasty.HUnit
import Text.RawString.QQ
import Data.Either

-- /admin/products.json
products :: String
products = [r| 
{  
   "products":[  
      {  
         "body_html":"This is a bar product",
         "created_at":"2014-12-15T18:37:12-05:00",
         "handle":"bar",
         "id":387149277,
         "product_type":"BarType",
         "published_at":"2014-12-15T18:37:00-05:00",
         "published_scope":"global",
         "template_suffix":null,
         "title":"Bar",
         "updated_at":"2014-12-15T18:38:01-05:00",
         "vendor":"BarVendor",
         "tags":"BarTag01, BarTag02",
         "variants":[  
            {  
               "barcode":"",
               "compare_at_price":"0.99",
               "created_at":"2014-12-15T18:37:12-05:00",
               "fulfillment_service":"manual",
               "grams":0,
               "id":1010857705,
               "inventory_management":null,
               "inventory_policy":"deny",
               "option1":"Default Title",
               "option2":null,
               "option3":null,
               "position":1,
               "price":"9.99",
               "product_id":387149277,
               "requires_shipping":true,
               "sku":"12345Bar12345",
               "taxable":true,
               "title":"Default Title",
               "updated_at":"2014-12-15T18:38:01-05:00",
               "inventory_quantity":1,
               "old_inventory_quantity":1,
               "image_id":null
            }
         ],
         "options":[  
            {  
               "id":458127421,
               "name":"Title",
               "position":1,
               "product_id":387149277
            }
         ],
         "images":[  
            {  
               "created_at":"2014-12-15T18:37:13-05:00",
               "id":1083243241,
               "position":1,
               "product_id":387149277,
               "updated_at":"2014-12-15T18:37:13-05:00",
               "src":"https://cdn.shopify.com/s/files/1/0725/0387/products/20140518072131_Placeholder_fc4b504e-9d7e-48f2-99a1-5892e9e33605.png?v=1418686633",
               "variant_ids":[  

               ]
            }
         ],
         "image":{  
            "created_at":"2014-12-15T18:37:13-05:00",
            "id":1083243241,
            "position":1,
            "product_id":387149277,
            "updated_at":"2014-12-15T18:37:13-05:00",
            "src":"https://cdn.shopify.com/s/files/1/0725/0387/products/20140518072131_Placeholder_fc4b504e-9d7e-48f2-99a1-5892e9e33605.png?v=1418686633",
            "variant_ids":[  

            ]
         }
      },
      {  
         "body_html":"This is a foo product",
         "created_at":"2014-12-15T18:36:52-05:00",
         "handle":"foo",
         "id":387149097,
         "product_type":"FooType",
         "published_at":"2014-12-15T18:35:00-05:00",
         "published_scope":"global",
         "template_suffix":null,
         "title":"Foo",
         "updated_at":"2014-12-15T18:36:52-05:00",
         "vendor":"FooVendor",
         "tags":"FooTag, FooTag2",
         "variants":[  
            {  
               "barcode":"",
               "compare_at_price":"0.99",
               "created_at":"2014-12-15T18:36:52-05:00",
               "fulfillment_service":"manual",
               "grams":0,
               "id":1010856917,
               "inventory_management":null,
               "inventory_policy":"deny",
               "option1":"Default Title",
               "option2":null,
               "option3":null,
               "position":1,
               "price":"9.99",
               "product_id":387149097,
               "requires_shipping":true,
               "sku":"12345Foo12345",
               "taxable":true,
               "title":"Default Title",
               "updated_at":"2014-12-15T18:36:52-05:00",
               "inventory_quantity":1,
               "old_inventory_quantity":1,
               "image_id":null
            }
         ],
         "options":[  
            {  
               "id":458127217,
               "name":"Title",
               "position":1,
               "product_id":387149097
            }
         ],
         "images":[  
            {  
               "created_at":"2014-12-15T18:36:52-05:00",
               "id":1083242757,
               "position":1,
               "product_id":387149097,
               "updated_at":"2014-12-15T18:36:52-05:00",
               "src":"https://cdn.shopify.com/s/files/1/0725/0387/products/20140518072131_Placeholder.png?v=1418686612",
               "variant_ids":[  

               ]
            }
         ],
         "image":{  
            "created_at":"2014-12-15T18:36:52-05:00",
            "id":1083242757,
            "position":1,
            "product_id":387149097,
            "updated_at":"2014-12-15T18:36:52-05:00",
            "src":"https://cdn.shopify.com/s/files/1/0725/0387/products/20140518072131_Placeholder.png?v=1418686612",
            "variant_ids":[  

            ]
         }
      }
   ]
}
|]

-- /admin/products/387149277.json
product :: String
product = [r|
{  
   "product":{  
      "body_html":"This is a bar product",
      "created_at":"2014-12-15T18:37:12-05:00",
      "handle":"bar",
      "id":387149277,
      "product_type":"BarType",
      "published_at":"2014-12-15T18:37:00-05:00",
      "published_scope":"global",
      "template_suffix":null,
      "title":"Bar",
      "updated_at":"2014-12-15T18:38:01-05:00",
      "vendor":"BarVendor",
      "tags":"BarTag01, BarTag02",
      "variants":[  
         {  
            "barcode":"",
            "compare_at_price":"0.99",
            "created_at":"2014-12-15T18:37:12-05:00",
            "fulfillment_service":"manual",
            "grams":0,
            "id":1010857705,
            "inventory_management":null,
            "inventory_policy":"deny",
            "option1":"Default Title",
            "option2":null,
            "option3":null,
            "position":1,
            "price":"9.99",
            "product_id":387149277,
            "requires_shipping":true,
            "sku":"12345Bar12345",
            "taxable":true,
            "title":"Default Title",
            "updated_at":"2014-12-15T18:38:01-05:00",
            "inventory_quantity":1,
            "old_inventory_quantity":1,
            "image_id":null
         }
      ],
      "options":[  
         {  
            "id":458127421,
            "name":"Title",
            "position":1,
            "product_id":387149277
         }
      ],
      "images":[  
         {  
            "created_at":"2014-12-15T18:37:13-05:00",
            "id":1083243241,
            "position":1,
            "product_id":387149277,
            "updated_at":"2014-12-15T18:37:13-05:00",
            "src":"https://cdn.shopify.com/s/files/1/0725/0387/products/20140518072131_Placeholder_fc4b504e-9d7e-48f2-99a1-5892e9e33605.png?v=1418686633",
            "variant_ids":[  

            ]
         }
      ],
      "image":{  
         "created_at":"2014-12-15T18:37:13-05:00",
         "id":1083243241,
         "position":1,
         "product_id":387149277,
         "updated_at":"2014-12-15T18:37:13-05:00",
         "src":"https://cdn.shopify.com/s/files/1/0725/0387/products/20140518072131_Placeholder_fc4b504e-9d7e-48f2-99a1-5892e9e33605.png?v=1418686633",
         "variant_ids":[  

         ]
      }
   }
}
|]

wyrdDistroProduct :: String
wyrdDistroProduct = [r|
  {
    "product": {
        "body_html": "\u003Cp\u003E\u003Cspan style=\"line-height: 1.2;\"\u003ELimited to 30 copies. Top noise tape of 2013.\u003C/span\u003E\u003C/p\u003E",
        "created_at": "2014-02-15T09:48:53-05:00",
        "handle": "termina-let-this-rotting-world-empty",
        "id": 240385833,
        "product_type": "Cassette",
        "published_at": "2014-02-15T09:48:53-05:00",
        "published_scope": "global",
        "template_suffix": null,
        "title": "Termina - Let This Rotting World Empty",
        "updated_at": "2014-12-20T18:59:28-05:00",
        "vendor": "Magic Mother",
        "tags": "cassette, experimental, Montréal, quebec",
        "variants": [{
            "barcode": null,
            "compare_at_price": null,
            "created_at": "2014-02-15T09:48:53-05:00",
            "fulfillment_service": "manual",
            "grams": 65,
            "id": 545421333,
            "inventory_management": "shopify",
            "inventory_policy": "deny",
            "option1": "Cassette",
            "option2": null,
            "option3": null,
            "position": 1,
            "price": "7.00",
            "product_id": 240385833,
            "requires_shipping": true,
            "sku": "WC-CASS-054075",
            "taxable": true,
            "title": "Cassette",
            "updated_at": "2014-12-20T18:59:28-05:00",
            "inventory_quantity": 0,
            "old_inventory_quantity": 0,
            "image_id": null
        }],
        "options": [{
            "id": 285872941,
            "name": "Title",
            "position": 1,
            "product_id": 240385833
        }],
        "images": [{
            "created_at": "2014-03-12T23:49:02-04:00",
            "id": 586546377,
            "position": 1,
            "product_id": 240385833,
            "updated_at": "2014-03-12T23:49:02-04:00",
            "src": "https://cdn.shopify.com/s/files/1/0250/7563/products/Wyrd_Distro-Termina-Let_This_Rotting_World_Empty-new.JPG?v=1394682542",
            "variant_ids": []
        }],
        "image": {
            "created_at": "2014-03-12T23:49:02-04:00",
            "id": 586546377,
            "position": 1,
            "product_id": 240385833,
            "updated_at": "2014-03-12T23:49:02-04:00",
            "src": "https://cdn.shopify.com/s/files/1/0250/7563/products/Wyrd_Distro-Termina-Let_This_Rotting_World_Empty-new.JPG?v=1394682542",
            "variant_ids": []
        }
    }
}
|]
