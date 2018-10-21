{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid
import Lucid.Base (ToHtml)
import Prelude hiding (Word)
import Data.String.Conversions

import Helper

type API = Capture "chars" String
        :> Capture "known" String
        :> Get '[JSON, HTML] Words

newtype Words = Result [String]
        deriving (ToJSON)

instance ToHtml Words where
    toHtml (Result words) = do
        head_ (title_ "Possible words")
        body_ $ do
           with div_ [id_ "header"] "Words"
           p_ "These are the possible words that fit in the crossword."
           ul_ $ mapM_ (li_ . toHtml) words

    toHtmlRaw = toHtml

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = jsonQuery

jsonQuery :: String -> String -> Handler Words
jsonQuery chars known = return . Result . map cs $ generateValid chars known
