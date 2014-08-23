{-# LANGUAGE OverloadedStrings #-}

module Main where

import Shopify.Types (Image(Image))

main :: IO ()
main = do
  let x = (Image "cool")
  putStrLn "cool"
