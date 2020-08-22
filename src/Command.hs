{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Command where

import System.Random (randomRIO)

import Control.Monad (when, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Text.Read (readMaybe)

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
    | otherwise = case messageCommand msg of
                    "coinflip" -> coinflip msg

isCommand :: Message -> Bool
isCommand = T.isPrefixOf "!" . T.toLower . messageText

messageCommand :: Message -> T.Text
messageCommand = T.tail . head . T.words . messageText

messageCommandArgs :: Message -> [T.Text]
messageCommandArgs = tail . T.words . messageText

isValidCommand :: Message -> Bool
isValidCommand = (`elem` commands) . messageCommand

commands :: [T.Text]
commands = ["coinflip", "8ball"]

coinflip :: Message -> DiscordHandler ()
coinflip msg = case messageCommandArgs msg of 
    -- No args is a singular flip
    [] -> do
        flip <- liftIO $ randomRIO (1 :: Int, 2)
        let text = T.append "Coin flipped: " $ if flip == 1
                then "Heads"
                else "Tails"
        _ <- restCall $ R.CreateMessage (messageChannel msg) text
        _ <- restCall $ R.CreateReaction (messageChannel msg, messageId msg) "white_check_mark"
        pure ()

    -- Singular arg will be a defined number of rolls
    [x] -> case (readMaybe . T.unpack $ x) :: Maybe Int of
        Just n -> do -- Valid integer - perform n rolls
            _ <- restCall $ R.CreateReaction (messageChannel msg, messageId msg) "white_check_mark"
            rolls <- mapM (\_ -> liftIO $ randomRIO (0 :: Int, 1)) [1..n]
            let heads = sum rolls
            _ <- restCall $ R.CreateMessage (messageChannel msg) (T.concat ["Of ", x, " rolls ", T.pack . show $ heads, " were heads"])
            pure ()

        Nothing -> do -- Invalid Integer
            _ <- restCall $ R.CreateReaction (messageChannel msg, messageId msg) "interrobang"
            _ <- restCall $ R.CreateMessage (messageChannel msg) "Invalid Quantity!" 
            pure ()

    -- All other patterns are invalid
    _ -> do
        _ <- restCall $ R.CreateReaction (messageChannel msg, messageId msg) "interrobang"
        _ <- restCall $ R.CreateMessage (messageChannel msg) "Invalid Arguments!"
        pure ()
