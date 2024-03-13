module Hardcoded (host, port, portNumber, database, user, password) where

import Data.String

host :: String
host = "127.0.0.1"

port :: String
port = "5432"

portNumber :: (Integral i) => i
portNumber = 5432

database :: String
database = "mydatabase"

user :: String
user = "myuser"

password :: String
password = "mypassword"