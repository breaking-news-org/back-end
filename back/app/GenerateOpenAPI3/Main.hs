module Main where

import API.OpenAPI3 (writeSpec)

main :: IO ()
main = writeSpec "static/API/v1.yaml"