{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Db where

import Prelude
import Data.Int
import Data.Vector
import Data.Text
import Data.Functor.Contravariant
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.TH as TH
import DbConnection


userSentencesStatement :: Statement Int32 (Vector Text)
userSentencesStatement =
  [TH.vectorStatement|
      select sentence :: text from sentences where author_id = $1 :: int                
  |]


userSentencesSession :: Int32 -> Session (Vector Text)
userSentencesSession author_id = Session.statement author_id userSentencesStatement
  

getUserSentences :: Int32 -> IO (Either Session.SessionError (Vector Text))
getUserSentences author_id = do
  Right connection <- getConnection
  Session.run (userSentencesSession author_id) connection


addSentenceStatement :: Statement (Int32, Text) ()
addSentenceStatement =
  [TH.resultlessStatement|
      insert into sentences (author_id, sentence) values ($1 :: int, $2 :: text )
  |]

addSentenceSession :: Int32 -> Text -> Session ()
addSentenceSession author_id sentence = Session.statement (author_id, sentence) addSentenceStatement


addSentence :: Int32 -> Text -> IO (Either Session.SessionError ())
addSentence author_id sentence = do
  Right connection <- getConnection
  Session.run (addSentenceSession author_id sentence) connection
  
    
