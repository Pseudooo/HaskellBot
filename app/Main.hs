module Main where

import Data.Text (pack)
import Bot

main :: IO ()
main = do
    token <- readFile "secret.txt"
    startBot $ pack token