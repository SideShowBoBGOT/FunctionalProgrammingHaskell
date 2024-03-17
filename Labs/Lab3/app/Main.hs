{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))
import Hardcoded
import Data.String (fromString)
import Data.ByteString ()
import Database.PostgreSQL.Simple (
  query, query_, fromOnly, FromRow, ToRow)
import Database.PostgreSQL.Simple.Internal (
  connectHost, connectUser, connectDatabase,
  connectPassword, Connection, connect,
  defaultConnectInfo)
import Database.PostgreSQL.Simple.Types (Only(Only), Query)
import GHC.Show()
import GHC.Generics (Generic)

data Author = Author {
  authorId :: Integer,
  authorName :: String
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data BookAuthor = BookAuthor {
  bookAuthorId :: Integer,
  bookId :: Integer,
  authorId :: Integer
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data ArticleAuthor = ArticleAuthor {
  articleAuthorId :: Integer,
  articleId :: Integer,
  authorId :: Integer
} deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

data ThesisAuthor = ThesisAuthor {
  thesisAuthorId :: Integer,
  thesisId :: Integer,
  authorId :: Integer
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
    bookPublisherId :: Integer,
    bookYear :: Integer
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
  cmd <- execParser programParser
  executeCommand cmd connection

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
  let queryArticles = toQuery "SELECT id FROM articles WHERE title = ?"
  articles :: [Only Integer] <- query connection queryArticles (Only name)
  let qBooks = toQuery "SELECT id FROM books WHERE title = ?"
  books :: [Only Integer] <- query connection qBooks (Only name)
  let qTheses = toQuery "SELECT id FROM theses WHERE title = ?"
  theses :: [Only Integer] <- query connection qTheses (Only name)

  let showOnly = show . fromOnly
  mapM_ (\textId -> print $ "Article: " ++ showOnly textId) articles
  mapM_ (\textId -> print $ "Book: " ++ showOnly textId) books
  mapM_ (\textId -> print $ "Thesis: " ++ showOnly textId) theses

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
                        "authors ON book_authors.author_id = authors.id " ++
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
                        "thesis_authors ON theses.id = thesis_authors.thesis_id " ++
                    "JOIN " ++
                        "authors ON thesis_authors.author_id = authors.id " ++
                    "WHERE " ++
                        "authors.name = ?;"
  articles :: [Article] <- query connection qArticles (Only name)
  books :: [Book] <- query connection qBooks (Only name)
  theses :: [Thesis] <- query connection qTheses (Only name)
  mapM_ (\v -> print $ "Article: " ++ show v) articles
  mapM_ (\v -> print $ "Book: " ++ show v) books
  mapM_ (\v -> print $ "Thesis: " ++ show v) theses

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


queryAllAuthors :: Connection -> IO ()
queryAllAuthors connection = do
  let q = toQuery "SELECT id, name FROM authors;"
  authors :: [Author] <- query_ connection q
  mapM_ print authors

queryAllCities :: Connection -> IO ()
queryAllCities connection = do
  let q = toQuery "SELECT id, name FROM cities;"
  vals :: [City] <- query_ connection q
  mapM_ print vals

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

queryAllBooks :: Connection -> IO ()
queryAllBooks connection = do
  let q = toQuery "SELECT id, title, city_id, publisher_id, year name FROM books;"
  vals :: [Book] <- query_ connection q
  mapM_ print vals

queryAllBookAuthors :: Connection -> IO ()
queryAllBookAuthors connection = do
  let q = toQuery "SELECT id, bookId, author_id name FROM book_authors;"
  vals :: [BookAuthor] <- query_ connection q
  mapM_ print vals

queryAllArticles :: Connection -> IO ()
queryAllArticles connection = do
  let q = toQuery "SELECT id, title, journal_id, issue, year, pages_start, pages_end name FROM articles;"
  vals :: [Article] <- query_ connection q
  mapM_ print vals

queryAllArticleAuthors :: Connection -> IO ()
queryAllArticleAuthors connection = do
  let q = toQuery "SELECT id, article_id, author_id name FROM article_authors;"
  vals :: [ArticleAuthor] <- query_ connection q
  mapM_ print vals

queryAllTheses :: Connection -> IO ()
queryAllTheses connection = do
  let q = toQuery "SELECT id, title, city_id, issue, conference_id, year, pages_start, pages_end name FROM theses;"
  vals :: [Thesis] <- query_ connection q
  mapM_ print vals

queryAllThesisAuthors :: Connection -> IO ()
queryAllThesisAuthors connection = do
  let q = toQuery "SELECT id, thesis_id, author_id name FROM thesis_authors;"
  vals :: [ThesisAuthor] <- query_ connection q
  mapM_ print vals







data Command
  = TextTypesByName String
  | CountAllTextTypes
  | AllTextSingleAuthored String

  | AllAuthors
  | AllCities
  | AllPublishers
  | AllConferences
  | AllJournals
  | AllBooks
  | AllBookAuthors
  | AllArticles
  | AllArticleAuthors
  | AllTheses
  | AllThesisAuthors

  | CreateAuthor String
  | CreateCity String
  | CreatePublisher String
  | CreateConference String
  | CreateJournal String
  | CreateBook {
    bookTitle :: String,
    bookCityId :: Integer,
    bookPublisherId :: Integer,
    bookYear :: Integer
  }
  | CreateBookAuthor {
    bookId :: Integer,
    authorId :: Integer
  }
  | CreateArticle {
    articleTitle :: String,
    articleJournalId :: Integer,
    articleIssue :: Int,
    articleYear :: Int,
    articlePagesStart :: Int,
    articlePagesEnd :: Int
  }
  | CreateArticleAuthor {
    articleId :: Integer,
    authorId :: Integer
  }
  | CreateThesis {
    thesisTitle :: String,
    thesisCityId :: Integer,
    thesisConferenceId :: Integer,
    thesisYear :: Int,
    thesisPagesStart :: Int,
    thesisPagesEnd :: Int
  }
  | CreateThesisAuthor {
     thesisId :: Integer,
     authorId :: Integer
  }

  deriving Show

cmdTextTypesByName :: Parser Command
cmdTextTypesByName = TextTypesByName
  <$> argument str (metavar "STRING" <> help "Name of text")

cmdAllTextSingleAuthored :: Parser Command
cmdAllTextSingleAuthored = AllTextSingleAuthored
  <$> argument str (metavar "STRING" <> help "author name")

cmdCountAllTextTypes :: Parser Command
cmdCountAllTextTypes = pure CountAllTextTypes

cmdAllAuthors :: Parser Command
cmdAllAuthors = pure AllAuthors

cmdAllCities :: Parser Command
cmdAllCities = pure AllCities

cmdAllPublishers :: Parser Command
cmdAllPublishers = pure AllPublishers

cmdAllConferences :: Parser Command
cmdAllConferences = pure AllConferences

cmdAllJournals :: Parser Command
cmdAllJournals = pure AllJournals

cmdAllBooks :: Parser Command
cmdAllBooks = pure AllBooks

cmdAllBookAuthors :: Parser Command
cmdAllBookAuthors = pure AllBookAuthors

cmdAllArticles :: Parser Command
cmdAllArticles = pure AllArticles

cmdAllArticleAuthors :: Parser Command
cmdAllArticleAuthors = pure AllArticleAuthors

cmdAllTheses :: Parser Command
cmdAllTheses = pure AllTheses

cmdAllThesisAuthors :: Parser Command
cmdAllThesisAuthors = pure AllThesisAuthors

cmdCreateAuthor :: Parser Command
cmdCreateAuthor = CreateAuthor
  <$> argument str (metavar "STRING" <> help "Author name")

cmdCreateCity :: Parser Command
cmdCreateCity = CreateAuthor
  <$> argument str (metavar "STRING" <> help "City name")

cmdCreatePublisher :: Parser Command
cmdCreatePublisher = CreatePublisher
  <$> argument str (metavar "STRING" <> help "Publisher name")

cmdCreateConference :: Parser Command
cmdCreateConference = CreateConference
  <$> argument str (metavar "STRING" <> help "Conference name")

cmdCreateJournal :: Parser Command
cmdCreateJournal = CreateJournal
  <$> argument str (metavar "STRING" <> help "Journal name")

cmdCreateBook :: Parser Command
cmdCreateBook = CreateBook
  <$> argument str (metavar "STRING" <> help "Book title")
  <*> argument auto (metavar "INTEGER" <> help "City id")
  <*> argument auto (metavar "INTEGER" <> help "Publisher id")
  <*> argument auto (metavar "INTEGER" <> help "Year")

cmdCreateBookAuthor :: Parser Command
cmdCreateBookAuthor = CreateBookAuthor
  <$> argument auto (metavar "INTEGER" <> help "Book id")
  <*> argument auto (metavar "INTEGER" <> help "Author id")

cmdCreateArticle :: Parser Command
cmdCreateArticle = CreateArticle
  <$> argument str (metavar "STRING" <> help "Article title")
  <*> argument auto (metavar "INTEGER" <> help "Journal id")
  <*> argument auto (metavar "INTEGER" <> help "Issue")
  <*> argument auto (metavar "INTEGER" <> help "Year")
  <*> argument auto (metavar "INTEGER" <> help "Pages start")
  <*> argument auto (metavar "INTEGER" <> help "Pages end")

cmdCreateArticleAuthor :: Parser Command
cmdCreateArticleAuthor = CreateArticleAuthor
  <$> argument auto (metavar "INTEGER" <> help "Article id")
  <*> argument auto (metavar "INTEGER" <> help "Author id")

cmdCreateThesis :: Parser Command
cmdCreateThesis = CreateThesis
  <$> argument str (metavar "STRING" <> help "Article title")
  <*> argument auto (metavar "INTEGER" <> help "City id")
  <*> argument auto (metavar "INTEGER" <> help "Conference id")
  <*> argument auto (metavar "INTEGER" <> help "Year")
  <*> argument auto (metavar "INTEGER" <> help "Pages start")
  <*> argument auto (metavar "INTEGER" <> help "Pages end")

cmdCreateThesisAuthor :: Parser Command
cmdCreateThesisAuthor = CreateThesisAuthor
  <$> argument auto (metavar "INTEGER" <> help "Thesis id")
  <*> argument auto (metavar "INTEGER" <> help "Thesis id")



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
    <> command "all_authors" (info cmdAllAuthors (progDesc "Print all authors"))
    <> command "all_cities" (info cmdAllCities (progDesc "Print all cities"))
    <> command "all_publishers" (info cmdAllPublishers (progDesc "Print all publishers"))
    <> command "all_conferences" (info cmdAllConferences (progDesc "Print all conferences"))
    <> command "all_journals" (info cmdAllJournals (progDesc "Print all journals"))
    <> command "all_books" (info cmdAllBooks (progDesc "Print all books"))
    <> command "all_book_authors" (info cmdAllBookAuthors (progDesc "Print all book authors"))
    <> command "all_articles" (info cmdAllArticles (progDesc "Print all articles"))
    <> command "all_article_authors" (info cmdAllArticleAuthors (progDesc "Print all article authors"))
    <> command "all_theses" (info cmdAllTheses (progDesc "Print all theses"))
    <> command "all_thesis_authors" (info cmdAllThesisAuthors (progDesc "Print all thesis authors"))
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
executeCommand CountAllTextTypes connection = queryCountAllTextTypes connection

executeCommand AllAuthors connection = queryAllAuthors connection
executeCommand AllCities connection = queryAllCities connection
executeCommand AllPublishers connection = queryAllPublishers connection
executeCommand AllConferences connection = queryAllConferences connection
executeCommand AllJournals connection = queryAllJournals connection
executeCommand AllBooks connection = queryAllBooks connection
executeCommand AllBookAuthors connection = queryAllBookAuthors connection
executeCommand AllArticles connection = queryAllArticles connection
executeCommand AllArticleAuthors connection = queryAllArticleAuthors connection
executeCommand AllTheses connection = queryAllTheses connection
executeCommand AllThesisAuthors connection = queryAllThesisAuthors connection

