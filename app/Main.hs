{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Main where

import Control.Monad (when, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = do
    token <- TIO.readFile "secret.txt"
    TIO.putStrLn $ T.append "Loaded Token: " token

    t <- runDiscord $ def
        { discordToken = token
        , discordOnStart = startHandler
        , discordOnEnd = liftIO $ putStrLn "Ended"
        , discordOnEvent = eventHandler
        , discordOnLog = \s -> TIO.putStrLn s >> putStrLn ""
        }
    TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c:_) -> do _ <- restCall $ R.CreateMessage (channelId c) "Hello! I will reply to pings with pongs"
                  pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (4 * 10^(6 :: Int))
        _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
        pure ()
      _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageText