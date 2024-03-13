{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

--import Database.PostgreSQL.Simple
import Options.Applicative
import Data.Semigroup ((<>))
import Lib
import Hardcoded
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.ByteString ()
import Database.PostgreSQL.Simple (query, query_, fromOnly)
import Database.PostgreSQL.Simple.Internal (connectHost, connectUser, connectDatabase, connectPassword, Connection, connect, defaultConnectInfo)
import Database.PostgreSQL.Simple.Types (Only(Only), Query(Query))
import GHC.Show (showString)

data Author = Author {
  authorId :: Integer,
  authorName :: String
}

data City = City {
  cityId :: Integer,
  cityName :: String
}

data Book = Book {
    bookAuthorsIds :: [Integer],
    bookTitle :: String,
    bookCityId :: Integer,
    bookPublisher :: String,
    bookYear :: Int
}

data Article = Article {
    articleAuthorsIds :: [Integer],
    articleTitle :: String,
    articleJournal :: String,
    articleYear :: Int,
    articleIssue :: Int,
    articlePages :: (Int, Int)
}

data Thesis = Thesis {
    thesisAuthors :: [String],
    thesisTitle :: String,
    thesisConference :: String,
    thesisCity :: City,
    thesisYear :: Int,
    thesisPages :: (Int, Int)
}

data Command
  = Hello { message :: String }
  | PrintNumber { number :: Int }
  deriving Show

helloCommand :: Parser Command
helloCommand = Hello
  <$> strArgument (metavar "MESSAGE" <> help "Hello message")
  
printNumberCommand = PrintNumber
  <$> argument auto (metavar "NUMBER" <> help "A number")

commandParser :: Parser Command
commandParser = subparser
  (
    command "hello" (info helloCommand (progDesc "Say hello"))
    <> command "print_number" (info printNumberCommand (progDesc "print number command"))  
  )
  
programParser :: ParserInfo Command
programParser = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "Test program for optparse-applicative"
  <> header "test-program - a test for optparse-applicative"
  )


main :: IO ()
main = do
  connection <- getConnection
  putStrLn "Hello"




getConnection :: IO Connection
getConnection =
  connect
    $ defaultConnectInfo
      { connectHost = Hardcoded.host
      , connectDatabase = Hardcoded.database
      , connectUser = Hardcoded.user
      , connectPassword = Hardcoded.password
      }

toQuery :: String -> Query
toQuery = fromString

printIdType :: (Integer, Text) -> IO ()
printIdType (id, textType) = do
    putStrLn (show id ++ ", " ++ show textType)

queryTextTypesByName :: Connection -> String -> IO ()
queryTextTypesByName connection name = do
  let qOne = toQuery "SELECT id, 'Article' FROM articles WHERE name == ?"
  articles :: [(Integer, Text)] <- query connection qOne (Only name)
  let qTwo = toQuery "SELECT id, 'Book' FROM books WHERE name == ?"
  books :: [(Integer, Text)] <- query connection qTwo (Only name)
  let qThree = toQuery "SELECT id, 'Thesis' FROM theses WHERE name == ?"
  theses :: [(Integer, Text)] <- query connection qThree (Only name)
  
  mapM_ printIdType articles
  mapM_ printIdType books
  mapM_ printIdType theses

