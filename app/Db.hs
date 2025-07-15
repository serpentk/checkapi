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
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Setting as ConnectionSetting
import qualified Hasql.Connection.Setting.Connection as ConnectionSettingConnection
import qualified Hasql.TH as TH

-- sumAndDivModSession :: Int64 -> Int64 -> Int64 -> Session (Int64, Int64)
-- sumAndDivModSession a b c = do
--   -- Get the sum of a and b
--   sumOfAAndB <- Session.statement (a, b) sumStatement
--   -- Divide the sum by c and get the modulo as well
--   Session.statement (sumOfAAndB, c) divModStatement

-- sumStatement :: Statement (Int64, Int64) Int64
-- sumStatement =
--   [TH.singletonStatement|
--     select ($1 :: int8 + $2 :: int8) :: int8
--     |]

-- divModStatement :: Statement (Int64, Int64) (Int64, Int64)
-- divModStatement =
--   [TH.singletonStatement|
--     select
--       (($1 :: int8) / ($2 :: int8)) :: int8,
--       (($1 :: int8) % ($2 :: int8)) :: int8
--     |]

userSentencesStatement :: Statement Int32 (Vector Text)
userSentencesStatement =
  [TH.vectorStatement|
      select sentence :: text from sentences where author_id = $1 :: int                
  |]


userSentencesSession :: Int32 -> Session (Vector Text)
userSentencesSession author_id = Session.statement author_id userSentencesStatement
  

-- print_user_sentences :: Int32 -> Either Session.SessionError (Vector Text)
print_user_sentences author_id = do
  Right connection <- Connection.acquire connectionSettings
  Session.run (userSentencesSession author_id) connection
  where
    connectionSettings = [ConnectionSetting.connection $ ConnectionSettingConnection.string connstr]
    connstr = "host=localhost dbname=sentences user=sentence_admin password=123456 port=5432"
