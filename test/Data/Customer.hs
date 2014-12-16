module Test.Data.Customer where

import Test.QuickCheck
import Test.Tasty.HUnit
import Text.RawString.QQ
import Data.Either


-- /admin/customers.json
customers :: String
customers = [r| 
{  
   "customers":[  
      {  
         "accepts_marketing":true,
         "created_at":"2014-12-15T20:44:03-05:00",
         "email":"Jane@doe.com",
         "first_name":"Jane",
         "id":342306537,
         "last_name":"Doe",
         "last_order_id":null,
         "multipass_identifier":null,
         "note":"This is a note about Jane",
         "orders_count":0,
         "state":"disabled",
         "total_spent":"0.00",
         "updated_at":"2014-12-15T20:45:23-05:00",
         "verified_email":true,
         "tags":"Jane, JaneDoe, JaneTag",
         "last_order_name":null,
         "default_address":{  
            "address1":"123 Jane Street",
            "address2":"",
            "city":"JaneTown",
            "company":"Jane Company",
            "country":"Australia",
            "first_name":"Jane",
            "id":411521885,
            "last_name":"Doe",
            "phone":"0411111111",
            "province":"Australian Capital Territory",
            "zip":"2000",
            "name":"Jane Doe",
            "province_code":"ACT",
            "country_code":"AU",
            "country_name":"Australia",
            "default":true
         },
         "addresses":[  
            {  
               "address1":"123 Jane Street",
               "address2":"",
               "city":"JaneTown",
               "company":"Jane Company",
               "country":"Australia",
               "first_name":"Jane",
               "id":411521885,
               "last_name":"Doe",
               "phone":"0411111111",
               "province":"Australian Capital Territory",
               "zip":"2000",
               "name":"Jane Doe",
               "province_code":"ACT",
               "country_code":"AU",
               "country_name":"Australia",
               "default":true
            },
            {  
               "address1":"123 Fake Street",
               "address2":"Fake Address 2",
               "city":"FakeTown",
               "company":"Jane Jane",
               "country":"Canada",
               "first_name":"Jane the second",
               "id":411522549,
               "last_name":"Doe",
               "phone":"04111111111",
               "province":"Alberta",
               "zip":"2020",
               "name":"Jane the second Doe",
               "province_code":"AB",
               "country_code":"CA",
               "country_name":"Canada",
               "default":false
            }
         ]
      },
      {  
         "accepts_marketing":false,
         "created_at":"2014-12-15T20:40:14-05:00",
         "email":"steve@smith.com",
         "first_name":"Steve",
         "id":342305289,
         "last_name":"Smith",
         "last_order_id":null,
         "multipass_identifier":null,
         "note":"This is a note about Steve",
         "orders_count":0,
         "state":"disabled",
         "total_spent":"0.00",
         "updated_at":"2014-12-15T21:00:27-05:00",
         "verified_email":true,
         "tags":"Steve, SteveTag",
         "last_order_name":null,
         "default_address":{  
            "address1":"",
            "address2":"",
            "city":"",
            "company":"",
            "country":"Australia",
            "first_name":"Steve",
            "id":411520093,
            "last_name":"Smith",
            "phone":"",
            "province":"Australian Capital Territory",
            "zip":"",
            "name":"Steve Smith",
            "province_code":"ACT",
            "country_code":"AU",
            "country_name":"Australia",
            "default":true
         },
         "addresses":[  
            {  
               "address1":"",
               "address2":"",
               "city":"",
               "company":"",
               "country":"Australia",
               "first_name":"Steve",
               "id":411520093,
               "last_name":"Smith",
               "phone":"",
               "province":"Australian Capital Territory",
               "zip":"",
               "name":"Steve Smith",
               "province_code":"ACT",
               "country_code":"AU",
               "country_name":"Australia",
               "default":true
            }
         ]
      }
   ]
}
|]

-- /admin/customers/342306537/addresses.json
addresses :: String
addresses = [r|
{  
   "addresses":[  
      {  
         "address1":"123 Jane Street",
         "address2":"",
         "city":"JaneTown",
         "company":"Jane Company",
         "country":"Australia",
         "first_name":"Jane",
         "id":411521885,
         "last_name":"Doe",
         "phone":"0411111111",
         "province":"Australian Capital Territory",
         "zip":"2000",
         "name":"Jane Doe",
         "province_code":"ACT",
         "country_code":"AU",
         "country_name":"Australia",
         "default":true
      },
      {  
         "address1":"123 Fake Street",
         "address2":"Fake Address 2",
         "city":"FakeTown",
         "company":"Jane Jane",
         "country":"Canada",
         "first_name":"Jane the second",
         "id":411522549,
         "last_name":"Doe",
         "phone":"04111111111",
         "province":"Alberta",
         "zip":"2020",
         "name":"Jane the second Doe",
         "province_code":"AB",
         "country_code":"CA",
         "country_name":"Canada",
         "default":false
      }
   ]
}
|]

-- /admin/customers/342306537/addresses/411521885.json
address :: String
address = [r|
{  
   "customer_address":{  
      "address1":"123 Jane Street",
      "address2":"",
      "city":"JaneTown",
      "company":"Jane Company",
      "country":"Australia",
      "first_name":"Jane",
      "id":411521885,
      "last_name":"Doe",
      "phone":"0411111111",
      "province":"Australian Capital Territory",
      "zip":"2000",
      "name":"Jane Doe",
      "province_code":"ACT",
      "country_code":"AU",
      "country_name":"Australia",
      "default":true
   }
}
|]
