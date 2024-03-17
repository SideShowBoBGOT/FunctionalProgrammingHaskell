{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

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
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data City = City {
  cityId :: Integer,
  cityName :: String
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data Journal = Journal {
  journalId :: Integer,
  journalName :: String
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data Publisher = Publisher {
  publisherId :: Integer,
  publisherName :: String
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data Conference = Conference {
  conferenceId :: Integer,
  confereceName :: String
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

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

main :: IO ()
main = do
  connection <- getConnection
  command <- execParser programParser
  executeCommand command connection

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

queryAllTextSingleAuthored :: Connection -> String -> IO ()
queryAllTextSingleAuthored connection name = do
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

queryAllPublishers :: Connection -> IO ()
queryAllPublishers connection = do
  let q = toQuery "SELECT id, name FROM publishers;"
  publishers :: [Publisher] <- query_ connection q
  mapM_ print publishers

queryAllConferences :: Connection -> IO ()
queryAllConferences connection = do
  let q = toQuery "SELECT id, name FROM conferences;"
  conferences :: [Conference] <- query_ connection q
  mapM_ print conferences

queryAllJournals :: Connection -> IO ()
queryAllJournals connection = do
  let q = toQuery "SELECT id, name FROM journals;"
  journals :: [Journal] <- query_ connection q
  mapM_ print journals

queryCountAllTextTypes :: Connection -> IO ()
queryCountAllTextTypes connection = do
  let qArticles = toQuery "SELECT COUNT(*) FROM articles;"
  articles :: [Only Int] <- query_ connection qArticles
  let qBooks = toQuery "SELECT COUNT(*) FROM books;"
  books :: [Only Int] <- query_ connection qBooks
  let qTheses = toQuery "SELECT COUNT(*) FROM theses;"
  theses :: [Only Int] <- query_ connection qTheses
  print $ "Articles count: " ++ show (fromOnly $ head articles)
  print $ "Books count: " ++ show (fromOnly $ head books)
  print $ "Theses count: " ++ show (fromOnly $ head theses)

data Command
  = TextTypesByName String
  | AllTextSingleAuthored String
  | AllPublishers
  | AllConferences
  | AllJournals
  | CountAllTextTypes
  deriving Show

cmdTextTypesByName :: Parser Command
cmdTextTypesByName = TextTypesByName
  <$> strOption (metavar "STRING" <> help "Name of text")

cmdAllTextSingleAuthored :: Parser Command
cmdAllTextSingleAuthored = AllTextSingleAuthored
  <$> strOption (
    metavar "STRING"
    <> help ""
  )

cmdAllPublishers :: Parser Command
cmdAllPublishers = pure AllPublishers

cmdAllConferences :: Parser Command
cmdAllConferences = pure AllConferences

cmdAllJournals :: Parser Command
cmdAllJournals = pure AllJournals

cmdCountAllTextTypes :: Parser Command
cmdCountAllTextTypes = pure CountAllTextTypes

commandParser :: Parser Command
commandParser = subparser
  (
    command "text_types_by_name" (
      info cmdTextTypesByName (progDesc "Get all text types by name")
    )
    <> command "all_text_single_authored" (
      info cmdAllTextSingleAuthored (
        progDesc "Get names of all solely written texts of the provided author"
      )
    )
    <> command "count_all_text_types" (
      info cmdCountAllTextTypes(progDesc "Count all text types")
    )
    <> command "all_publishers" (info cmdAllPublishers (progDesc "Print all publishers"))
    <> command "all_conferences" (info cmdAllConferences (progDesc "Print all conferences"))
    <> command "all_journals" (info cmdAllJournals (progDesc "Print all journals"))
  )

programParser :: ParserInfo Command
programParser = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "Test program for optparse-applicative"
  <> header "test-program - a test for optparse-applicative"
  )

executeCommand :: Command -> Connection -> IO ()
executeCommand (TextTypesByName name) connection = queryTextTypesByName connection name
executeCommand (AllTextSingleAuthored name) connection = queryAllTextSingleAuthored connection name
executeCommand AllPublishers connection = queryAllPublishers connection
executeCommand AllConferences connection = queryAllConferences connection
executeCommand AllJournals connection = queryAllJournals connection
executeCommand CountAllTextTypes connection = queryCountAllTextTypes connection