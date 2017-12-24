{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment
import Jatko

main :: IO ()
main = getArgs >>= \case
  [path] -> build path
  _ -> fail "jatko PATH"
