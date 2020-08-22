module Main where

import Data.Text (pack)
import Bot

main :: IO ()
main = readFile "secret.txt" >>= startBot . pack