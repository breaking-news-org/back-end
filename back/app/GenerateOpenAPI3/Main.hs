module Main where

import API.OpenAPI3 (writeSpec)

main :: IO ()
main = writeSpec "API/v1.yaml"