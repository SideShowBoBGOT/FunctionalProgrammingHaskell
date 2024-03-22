module MyConnection (getConnection) where

import Data.String
import Database.PostgreSQL.Simple.Internal (Connection, connectHost, connectDatabase, connectUser, connectPassword, connect, defaultConnectInfo)

host :: String
host = "127.0.0.1"

database :: String
database = "mydatabase"

user :: String
user = "myuser"

password :: String
password = "mypassword"

getConnection :: IO Connection
getConnection =
  connect
    $ defaultConnectInfo
      { connectHost = MyConnection.host
      , connectDatabase = MyConnection.database
      , connectUser = MyConnection.user
      , connectPassword = MyConnection.password
      }