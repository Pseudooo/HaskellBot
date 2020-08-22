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

-- ####################################################### Core command stuff

{-
    This is the function that handles any message that starts with '!'
    but the bot won't respond to commands that are here
-}
execCommand :: Message -> DiscordHandler ()
execCommand msg
    -- Ignore Invalid commands
    | not $ isValidCommand msg = pure ()
    -- Valid commands follow
    -- Case statement used to associate cmd strings with functions
    | otherwise = case messageCommand msg of
                    "coinflip" -> coinflip msg


-- Check if a Message is a command (starts with !)
isCommand :: Message -> Bool
isCommand = T.isPrefixOf "!" . T.toLower . messageText

-- Given a message extract the command string !command arg1 arg
messageCommand :: Message -> T.Text
messageCommand = T.tail . head . T.words . messageText

-- Given a message extract the arguments
messageCommandArgs :: Message -> [T.Text]
messageCommandArgs = tail . T.words . messageText

-- Check if a given message is a valid and registered command
isValidCommand :: Message -> Bool
isValidCommand = (`elem` commands) . messageCommand

-- A string list of all commands
commands :: [T.Text]
commands = ["coinflip", "8ball"]

-- ####################################################### Command helpers

react :: T.Text -> Message -> DiscordHandler ()
react emoji msg = (restCall $ R.CreateReaction (messageChannel msg, messageId msg) emoji)
    >>= \_ -> pure ()

respond :: T.Text -> Message -> DiscordHandler ()
respond response msg = (restCall $ R.CreateMessage (messageChannel msg) response)
    >>= \_ -> pure ()

negReact :: Message -> DiscordHandler ()
negReact = react "interrobang"

posReact :: Message -> DiscordHandler () 
posReact = react "white_check_mark"

-- ####################################################### COMMAND IMPLEMENTATIONS

coinflip :: Message -> DiscordHandler ()
coinflip msg = case messageCommandArgs msg of 
    -- No args is a singular flip
    [] -> do
        flip <- liftIO $ randomRIO (1 :: Int, 2)
        let responseText = T.append "Coin flipped: " $ if flip == 1
                then "Heads"
                else "Tails"
        respond responseText msg >> posReact msg

    -- Singular arg will be a defined number of rolls
    -- Using case and readMaybe to check if a valid int
    [x] -> case (readMaybe . T.unpack $ x) :: Maybe Int of

        Just n -> do -- Valid integer - perform n rolls
            rolls <- mapM (\_ -> liftIO $ randomRIO (0 :: Int, 1)) [1..n]
            let heads = sum rolls
            posReact msg >> respond (T.concat ["Of ", x, " flips ", T.pack . show $ heads, " were heads"]) msg

        -- Invalid Integer given
        Nothing -> negReact msg >> respond "Invalid Quantity!" msg

    -- All other patterns are invalid
    _ -> negReact msg >> respond "Invalid Arguments" msg
