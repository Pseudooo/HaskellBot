{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Command where

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
    | otherwise = pure ()

isCommand :: Message -> Bool
isCommand = T.isPrefixOf "!" . T.toLower . messageText

messageCommand :: Message -> T.Text
messageCommand = T.tail . head . T.words . messageText

isValidCommand :: Message -> Bool
isValidCommand = (`elem` commands) . messageCommand

commands :: [T.Text]
commands = ["coinflip", "8ball"]
