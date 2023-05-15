module Main where

import API.OpenAPI3 (writeSpec)

main :: IO ()
main = writeSpec "pulumi/static/API/v1.yaml"