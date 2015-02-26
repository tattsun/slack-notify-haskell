{-# LANGUAGE OverloadedStrings #-}
-- | This module sends notification to slack.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.Slack.Notifier
-- >
-- > main = do
-- >   let config = defaultConfig { webhookurl = "https://hooks.slack.com/services/your/slack/webhookurl"
-- >                              , channel = Just "#general"
-- >                              , username = Just "haskell"
-- >                              }
-- >    notify config "Hello, World"
-- >    return ()
module Network.Slack.Notifier
       ( Config(..)
       , defaultConfig
       , notify
       ) where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8                 as BS
import           Data.List
import qualified Data.Map.Strict                       as M ()
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           Network
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal          ()
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Client.TLS
----------------------------------------------------------------------

-- | Configuration of notifying slack
data Config = Config { webhookurl :: String
                     , channel    :: Maybe T.Text
                     , username   :: Maybe T.Text
                     , iconurl    :: Maybe T.Text
                     , iconemoji  :: Maybe T.Text
                     }

defaultConfig :: Config
defaultConfig = Config { webhookurl = ""
                       , channel = Nothing
                       , username = Nothing
                       , iconurl = Nothing
                       , iconemoji = Nothing
                       }

encode :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
encode = (`BS.append` "}") . ("{" `BS.append`) .
         BS.concat . intersperse "," .
         map (\(k, v) -> BS.concat ["\"", k, "\"",":", "\"", v,"\""])

----------------------------------------------------------------------

-- | Notify text to slack.
-- Throws exception when the request fail.
notify :: Config -> T.Text -> IO (Either HttpException ())
notify conf body = go `catch` (\(e) -> return $ Left e)
  where
    mbl key (Just a) = [(key, T.encodeUtf8 a)]
    mbl _ _ = []
    safetext = T.concat . intersperse "\\\"" . T.splitOn "\"" .
               T.concat . intersperse "\\n" . T.split (=='\n')
    go = withSocketsDo $ withManager tlsManagerSettings $ \m -> do
      req <- parseUrl (webhookurl conf)
      void $ flip httpLbs m =<<
        (flip formDataBody req $
         [partBS "payload" $ encode $
          mbl "channel" (channel conf) ++
          mbl "username" (username conf) ++
          mbl "icon_url" (iconurl conf) ++
          mbl "icon_emoji" (iconemoji conf) ++
          [("text", T.encodeUtf8 . safetext $ body)]
         ]
        )
      return $ Right ()
