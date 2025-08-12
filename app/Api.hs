{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Api where
import Data.Int
import qualified Data.Vector
import Data.Text

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Db
import Control.Monad.IO.Class
-- * api

type SentencesApi =
  "author" :> Capture "authorId" Int32 :> Get '[JSON] [Sentence]

sentencesApi :: Proxy SentencesApi
sentencesApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ Prelude.show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve sentencesApi server

server :: Server SentencesApi
server =
  getSentencesByAuthorId

getSentencesByAuthorId :: Int32 -> Handler [Sentence]
getSentencesByAuthorId authorId = do
  dbData <- liftIO $ getUserSentences authorId
  case dbData of
    Right dataVec -> return [Sentence $ unpack x | x <- Data.Vector.toList dataVec]
    Left err -> throwError err500

data Sentence
 = Sentence {
   sentence :: String
   }
  deriving (Eq, Show, Generic)

instance ToJSON Sentence
instance FromJSON Sentence

