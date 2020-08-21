{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Command where

import System.Random (randomRIO)

import Control.Monad (when, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

execCommand :: Message -> DiscordHandler ()
execCommand msg
    -- Handle invalid cases
    | not $ isValidCommand msg = do
        _ <- restCall $ R.CreateReaction (messageChannel msg, messageId msg) "interrobang"
        _ <- restCall $ R.CreateMessage (messageChannel msg) "Get it right dickhead" 
        pure ()
    -- Valid commands follow
    | otherwise = (restCall $ R.CreateReaction (messageChannel msg, messageId msg) "white_check_mark")
        >> case messageCommand msg of
            "coinflip" -> coinflip msg

isCommand :: Message -> Bool
isCommand = T.isPrefixOf "!" . T.toLower . messageText

messageCommand :: Message -> T.Text
messageCommand = T.tail . head . T.words . messageText

isValidCommand :: Message -> Bool
isValidCommand = (`elem` commands) . messageCommand

commands :: [T.Text]
commands = ["coinflip", "8ball"]

coinflip :: Message -> DiscordHandler ()
coinflip msg = do
    flip <- liftIO $ randomRIO (1 :: Int, 2)
    let text = T.append "Coin flipped: " $ if flip == 1
            then "Heads"
            else "Tails"
    _ <- restCall $ R.CreateMessage (messageChannel msg) text
    pure ()
