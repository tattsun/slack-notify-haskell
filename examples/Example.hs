{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Network.Slack.Notifier

main = do
  let config = defaultConfig { webhookurl = "https://hooks.slack.com/services/your/slack/webhookurl"
                              , channel = Just "#general"
                              , username = Just "haskell"
                              }
  notify config "Hello, World"
  return ()
