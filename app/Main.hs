{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Main where

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import UnliftIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

main :: IO ()
main = do
    userFacingError <- runDiscord $ def
        { discordToken = "Bot NzQ2Mzk1ODQ1Mjg0MzMxNTIw"
        , discordOnEvent = eventHandler}
    TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
               _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
               _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower
