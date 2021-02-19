module Main where

import qualified Mems (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Mems.someFunc
