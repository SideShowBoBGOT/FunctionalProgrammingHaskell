{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

--import Database.PostgreSQL.Simple
import Options.Applicative
import Data.Semigroup ((<>))
import Lib
import Hardcoded
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.ByteString ()
import Database.PostgreSQL.Simple (
  query, query_, fromOnly, FromRow, ToRow)
import Database.PostgreSQL.Simple.Internal (
  connectHost, connectUser, connectDatabase,
  connectPassword, Connection, connect,
  defaultConnectInfo)
import Database.PostgreSQL.Simple.Types (Only(Only), Query(Query))
import GHC.Show (showString)
import GHC.Generics (Generic)

data Author = Author {
  authorId :: Integer,
  authorName :: String
}

data City = City {
  cityId :: Integer,
  cityName :: String
}

data Book = Book {
    bookId :: Integer,
    bookTitle :: String,
    bookCityId :: Integer,
    bookPublisher :: String,
    bookYear :: Int
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data Article = Article {
    articleId :: Integer,
    articleTitle :: String,
    articleJournalId :: Integer,
    articleIssue :: Int,
    articleYear :: Int,
    articlePagesStart :: Int,
    articlePagesEnd :: Int
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data Thesis = Thesis {
    thesisId :: Integer,
    thesisTitle :: String,
    thesisCityId :: Integer,
    thesisConferenceId :: Integer,
    thesisYear :: Int,
    thesisPagesStart :: Int,
    thesisPagesEnd :: Int
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

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
  let queryArticles = toQuery "SELECT id FROM articles WHERE name == ?"
  articles :: [Only Integer] <- query connection queryArticles (Only name)
  let qBooks = toQuery "SELECT id FROM books WHERE name == ?"
  books :: [Only Integer] <- query connection qBooks (Only name)
  let qTheses = toQuery "SELECT id, 'Thesis' FROM theses WHERE name == ?"
  theses :: [Only Integer] <- query connection qTheses (Only name)
  
  mapM_ (\id -> print $ "Article: " ++ show id) articles
  mapM_ (\id -> print $ "Book: " ++ show id) books
  mapM_ (\id -> print $ "Thesis: " ++ show id) theses

queryAllTextOfAuthor :: Connection -> String -> IO ()
queryAllTextOfAuthor connection name = do
  let qArticles = toQuery $ "SELECT " ++
                       "articles.id," ++
                       "articles.title," ++
                       "articles.journal_id," ++
                       "articles.issue," ++
                       "articles.year," ++
                       "articles.pages_start," ++
                       "articles.pages_end " ++
                   "FROM " ++
                       "articles " ++
                   "JOIN " ++
                       "article_authors ON articles.id = article_authors.article_id " ++
                   "JOIN " ++
                       "authors ON article_authors.author_id = authors.id " ++
                   "WHERE " ++
                       "authors.name = ?;"
  let qBooks = toQuery $ "SELECT " ++
                        "books.id," ++
                        "books.title," ++
                        "books.city_id," ++
                        "books.publisher_id, " ++
                        "books.year " ++
                    "FROM " ++
                        "books " ++
                    "JOIN " ++
                        "book_authors ON books.id = book_authors.book_id " ++
                    "JOIN " ++
                        "books ON book_authors.author_id = authors.id " ++
                    "WHERE " ++
                        "authors.name = ?;"
  let qTheses = toQuery $ "SELECT " ++
                        "theses.id," ++
                        "theses.title," ++
                        "theses.city_id," ++
                        "theses.conference_id," ++
                        "theses.year," ++
                        "theses.pages_start," ++
                        "theses.pages_end " ++
                    "FROM " ++
                        "theses " ++
                    "JOIN " ++
                        "thesis_authors ON books.id = thesis_authors.thesis_id " ++
                    "JOIN " ++
                        "theses ON thesis_authors.author_id = authors.id " ++
                    "WHERE " ++
                        "authors.name = ?;"
  articles :: [Article] <- query connection qArticles (Only name)
  books :: [Book] <- query connection qBooks (Only name)
  theses :: [Thesis] <- query connection qTheses (Only name)
  mapM_ (\v -> print $ "Article: " ++ show v) articles
  mapM_ (\v -> print $ "Book: " ++ show v) books
  mapM_ (\v -> print $ "Thesis: " ++ show v) theses


  
