{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module DbConnection where
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Setting as ConnectionSetting
import qualified Hasql.Connection.Setting.Connection as ConnectionSettingConnection

getConnection :: IO (Either Connection.ConnectionError Connection.Connection)
getConnection = Connection.acquire connectionSettings
  where
    connectionSettings = [ConnectionSetting.connection $ ConnectionSettingConnection.string connstr]
    connstr = "host=localhost dbname=sentences user=sentence_admin password=123456 port=5432"
