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
  query, query_, fromOnly, FromRow, ToRow, execute)
import Database.PostgreSQL.Simple.Internal (
  connectHost, connectUser, connectDatabase,
  connectPassword, Connection, connect,
  defaultConnectInfo)
import Database.PostgreSQL.Simple.Types (Only(Only), Query)
import GHC.Show()
import GHC.Generics (Generic)
import Data.Int (Int64)

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

queryCreateAuthor :: Connection -> String -> IO ()
queryCreateAuthor connection name = do
  let q = toQuery "INSERT INTO authors (name) VALUES (?) RETURNING id"
  ids :: [Only Int64] <- query connection q (Only name)
  mapM_ (\id -> print $ "Created author with id " ++ show (fromOnly id)) ids

queryCreateCity:: Connection -> String -> IO ()
queryCreateCity connection name = do
  let q = toQuery "INSERT INTO cities (name) VALUES (?) RETURNING id"
  ids :: [Only Int64] <- query connection q (Only name)
  mapM_ (\id -> print $ "Created city with id " ++ show (fromOnly id)) ids

queryCreatePublisher:: Connection -> String -> IO ()
queryCreatePublisher connection name = do
  let q = toQuery "INSERT INTO publishers (name) VALUES (?) RETURNING id"
  ids :: [Only Int64] <- query connection q (Only name)
  mapM_ (\id -> print $ "Created publisher with id " ++ show (fromOnly id)) ids

queryCreateConference:: Connection -> String -> IO ()
queryCreateConference connection name = do
  let q = toQuery "INSERT INTO conferences (name) VALUES (?) RETURNING id"
  ids :: [Only Int64] <- query connection q (Only name)
  mapM_ (\id -> print $ "Created conference with id " ++ show (fromOnly id)) ids

queryCreateJournal:: Connection -> String -> IO ()
queryCreateJournal connection name = do
  let q = toQuery "INSERT INTO journals (name) VALUES (?) RETURNING id"
  ids :: [Only Int64] <- query connection q (Only name)
  mapM_ (\id -> print $ "Created journal with id " ++ show (fromOnly id)) ids

queryCreateBook :: Connection -> String -> Integer -> Integer -> Integer -> IO ()
queryCreateBook connection title city_id publisher_id year = do
  let q = toQuery "INSERT INTO books (title, city_id, publisher_id, year) VALUES (?, ?, ?, ?) RETURNING id"
  ids :: [Only Int64] <- query connection q (title, city_id, publisher_id, year)
  mapM_ (\id -> print $ "Created book with id " ++ show (fromOnly id)) ids

queryCreateBookAuthor :: Connection -> Integer -> Integer -> IO ()
queryCreateBookAuthor connection bId aId = do
  let q = toQuery "INSERT INTO book_authors (book_id, author_id) VALUES (?, ?)"
  execute connection q (bId, aId)
  print "Created book-author"

queryCreateArticle :: Connection -> String -> Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
queryCreateArticle connection aTitle aJournalId aIssue aYear aPagesStart aPagesEnd = do
  let q = toQuery "INSERT INTO articles (title, journal_id, issue, year, pages_start, pages_end) VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
  ids :: [Only Int64] <- query connection q (aTitle, aJournalId, aIssue, aYear, aPagesStart, aPagesEnd)
  mapM_ (\id -> print $ "Created article with id " ++ show (fromOnly id)) ids

queryCreateArticleAuthor :: Connection -> Integer -> Integer -> IO ()
queryCreateArticleAuthor connection arId aId = do
  let q = toQuery "INSERT INTO article_authors (article_id, author_id) VALUES (?, ?)"
  execute connection q (arId, aId)
  print "Created article-author"

queryCreateThesis :: Connection -> String -> Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
queryCreateThesis connection tTitle tCityId tConferenceId tYear tPagesStart tPagesEnd = do
  let q = toQuery "INSERT INTO theses (title, city_id, conference_id, year, pages_start, pages_end) VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
  ids :: [Only Int64] <- query connection q (tTitle, tCityId, tConferenceId, tYear, tPagesStart, tPagesEnd)
  mapM_ (\id -> print $ "Created thesis with id " ++ show (fromOnly id)) ids

queryCreateThesisAuthor :: Connection -> Integer -> Integer -> IO ()
queryCreateThesisAuthor connection tId aId = do
  let q = toQuery "INSERT INTO thesis_authors (thesis_id, author_id) VALUES (?, ?)"
  execute connection q (tId, aId)
  print "Created thesis-author"


queryDeleteAuthor :: Connection -> Integer -> IO ()
queryDeleteAuthor connection id = do
  let q = toQuery "DELETE FROM authors WHERE id = ?"
  execute connection q (Only id)
  print "Deleted authors"

queryDeleteCity :: Connection -> Integer -> IO ()
queryDeleteCity connection id = do
  let q = toQuery "DELETE FROM cities WHERE id = ?"
  execute connection q (Only id)
  print "Deleted city"

queryDeletePublisher :: Connection -> Integer -> IO ()
queryDeletePublisher connection id = do
  let q = toQuery "DELETE FROM publishers WHERE id = ?"
  execute connection q (Only id)
  print "Deleted publisher"

queryDeleteConference :: Connection -> Integer -> IO ()
queryDeleteConference connection id = do
  let q = toQuery "DELETE FROM conferences WHERE id = ?"
  execute connection q (Only id)
  print "Deleted conference"

queryDeleteJournal :: Connection -> Integer -> IO ()
queryDeleteJournal connection id = do
  let q = toQuery "DELETE FROM journals WHERE id = ?"
  execute connection q (Only id)
  print "Deleted journal"

queryDeleteBook :: Connection -> Integer -> IO ()
queryDeleteBook connection id = do
  let q = toQuery "DELETE FROM books WHERE id = ?"
  execute connection q (Only id)
  print "Deleted book"

queryDeleteBookAuthor :: Connection -> Integer -> Integer -> IO ()
queryDeleteBookAuthor connection id aId = do
  let q = toQuery "DELETE FROM book_authors WHERE book_id = ? AND author_id = ?"
  execute connection q (id, aId)
  print "Deleted book-author"

queryDeleteArticle :: Connection -> Integer -> IO ()
queryDeleteArticle connection id = do
  let q = toQuery "DELETE FROM articles WHERE id = ?"
  execute connection q (Only id)
  print "Deleted article"

queryDeleteArticleAuthor :: Connection -> Integer -> Integer -> IO ()
queryDeleteArticleAuthor connection id aId = do
  let q = toQuery "DELETE FROM article_authors WHERE article_id = ? AND author_id = ?"
  execute connection q (id, aId)
  print "Deleted book-author"

queryDeleteThesis :: Connection -> Integer -> IO ()
queryDeleteThesis connection id = do
  let q = toQuery "DELETE FROM books WHERE id = ?"
  execute connection q (Only id)
  print "Deleted book"

queryDeleteThesisAuthor :: Connection -> Integer -> Integer -> IO ()
queryDeleteThesisAuthor connection id aId = do
  let q = toQuery "DELETE FROM book_authors WHERE thesis_id = ? AND author_id = ?"
  execute connection q (id, aId)
  print "Deleted thesis-author"

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
    articleIssue :: Integer,
    articleYear :: Integer,
    articlePagesStart :: Integer,
    articlePagesEnd :: Integer
  }
  | CreateArticleAuthor {
    articleId :: Integer,
    authorId :: Integer
  }
  | CreateThesis {
    thesisTitle :: String,
    thesisCityId :: Integer,
    thesisConferenceId :: Integer,
    thesisYear :: Integer,
    thesisPagesStart :: Integer,
    thesisPagesEnd :: Integer
  }
  | CreateThesisAuthor {
     thesisId :: Integer,
     authorId :: Integer
  }

  | DeleteAuthor Integer
  | DeleteCity Integer
  | DeletePublisher Integer
  | DeleteConference Integer
  | DeleteJournal Integer
  | DeleteBook Integer
  | DeleteBookAuthor Integer Integer
  | DeleteArticle Integer
  | DeleteArticleAuthor Integer Integer
  | DeleteThesis Integer
  | DeleteThesisAuthor Integer Integer


  deriving (Show, Generic)

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
  <$> argument str (metavar "STRING" <> help "Title")
  <*> argument auto (metavar "INTEGER" <> help "City id")
  <*> argument auto (metavar "INTEGER" <> help "Publisher id")
  <*> argument auto (metavar "INTEGER" <> help "Year")

cmdCreateBookAuthor :: Parser Command
cmdCreateBookAuthor = CreateBookAuthor
  <$> argument auto (metavar "INTEGER" <> help "Book id")
  <*> argument auto (metavar "INTEGER" <> help "Author id")

cmdCreateArticle :: Parser Command
cmdCreateArticle = CreateArticle
  <$> argument str (metavar "STRING" <> help "Title")
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
  <$> argument str (metavar "STRING" <> help "Title")
  <*> argument auto (metavar "INTEGER" <> help "City id")
  <*> argument auto (metavar "INTEGER" <> help "Conference id")
  <*> argument auto (metavar "INTEGER" <> help "Year")
  <*> argument auto (metavar "INTEGER" <> help "Pages start")
  <*> argument auto (metavar "INTEGER" <> help "Pages end")

cmdCreateThesisAuthor :: Parser Command
cmdCreateThesisAuthor = CreateThesisAuthor
  <$> argument auto (metavar "INTEGER" <> help "Thesis id")
  <*> argument auto (metavar "INTEGER" <> help "Author id")


cmdDeleteAuthor :: Parser Command
cmdDeleteAuthor = DeleteAuthor
  <$> argument auto (metavar "INTEGER" <> help "Author id")

cmdDeleteCity :: Parser Command
cmdDeleteCity = DeleteCity
  <$> argument auto (metavar "INTEGER" <> help "City id")

cmdDeletePublisher :: Parser Command
cmdDeletePublisher = DeletePublisher
  <$> argument auto (metavar "INTEGER" <> help "Publisher id")

cmdDeleteConference :: Parser Command
cmdDeleteConference = DeleteConference
  <$> argument auto (metavar "INTEGER" <> help "Conference id")

cmdDeleteJournal :: Parser Command
cmdDeleteJournal = DeleteJournal
  <$> argument auto (metavar "INTEGER" <> help "Journal id")

cmdDeleteBook :: Parser Command
cmdDeleteBook = DeleteBook
  <$> argument auto (metavar "INTEGER" <> help "Book id")

cmdDeleteBookAuthor :: Parser Command
cmdDeleteBookAuthor = DeleteBookAuthor
  <$> argument auto (metavar "INTEGER" <> help "Book id")
  <*> argument auto (metavar "INTEGER" <> help "Author id")

cmdDeleteArticle :: Parser Command
cmdDeleteArticle = DeleteArticle
  <$> argument auto (metavar "INTEGER" <> help "Article id")

cmdDeleteArticleAuthor :: Parser Command
cmdDeleteArticleAuthor = DeleteArticleAuthor
  <$> argument auto (metavar "INTEGER" <> help "Article id")
  <*> argument auto (metavar "INTEGER" <> help "Author id")

cmdDeleteThesis :: Parser Command
cmdDeleteThesis = DeleteThesis
  <$> argument auto (metavar "INTEGER" <> help "Thesis id")

cmdDeleteThesisAuthor :: Parser Command
cmdDeleteThesisAuthor = DeleteThesisAuthor
  <$> argument auto (metavar "INTEGER" <> help "Thesis id")
  <*> argument auto (metavar "INTEGER" <> help "Author id")


commandParser :: Parser Command
commandParser = subparser
  (
    command "text_types_by_name" (
      info (cmdTextTypesByName <**> helper) (progDesc "Get all text types by name")
    )
    <> command "all_text_single_authored" (
      info (cmdAllTextSingleAuthored <**> helper) (
        progDesc "Get names of all solely written texts of the provided author"
      )
    )
    <> command "count_all_text_types" (
      info (cmdCountAllTextTypes <**> helper) (progDesc "Count all text types")
    )
    <> command "all-authors" (info (cmdAllAuthors <**> helper) (progDesc "Print all authors"))
    <> command "all-cities" (info (cmdAllCities <**> helper) (progDesc "Print all cities"))
    <> command "all-publishers" (info (cmdAllPublishers <**> helper) (progDesc "Print all publishers"))
    <> command "all-conferences" (info (cmdAllConferences <**> helper) (progDesc "Print all conferences"))
    <> command "all-journals" (info (cmdAllJournals <**> helper) (progDesc "Print all journals"))
    <> command "all-books" (info (cmdAllBooks <**> helper) (progDesc "Print all books"))
    <> command "all-book-authors" (info (cmdAllBookAuthors <**> helper) (progDesc "Print all book authors"))
    <> command "all-articles" (info (cmdAllArticles <**> helper) (progDesc "Print all articles"))
    <> command "all-article-authors" (info (cmdAllArticleAuthors <**> helper) (progDesc "Print all article authors"))
    <> command "all-theses" (info (cmdAllTheses <**> helper) (progDesc "Print all theses"))
    <> command "all-thesis-authors" (info (cmdAllThesisAuthors <**> helper) (progDesc "Print all thesis authors"))

    <> command "create-author" (info (cmdCreateAuthor <**> helper) (progDesc "Create author"))
    <> command "create-city" (info (cmdCreateCity <**> helper) (progDesc "Create city"))
    <> command "create-publisher" (info (cmdCreatePublisher <**> helper) (progDesc "Create publisher"))
    <> command "create-conference" (info (cmdCreateConference <**> helper) (progDesc "Create conference"))
    <> command "create-journal" (info (cmdCreateJournal <**> helper) (progDesc "Create journal"))
    <> command "create-book" (info (cmdCreateBook <**> helper) (progDesc "Create book"))
    <> command "create-book-author" (info (cmdCreateBookAuthor <**> helper) (progDesc "Create book-author pair"))
    <> command "create-article" (info (cmdCreateArticle <**> helper) (progDesc "Create article"))
    <> command "create-article-author" (info (cmdCreateArticleAuthor <**> helper) (progDesc "Create article-author pair"))
    <> command "create-thesis" (info (cmdCreateThesis <**> helper) (progDesc "Create thesis"))
    <> command "create-thesis-author" (info (cmdCreateThesisAuthor <**> helper) (progDesc "Create thesis-author pair"))

    <> command "delete-author" (info (cmdDeleteAuthor <**> helper) (progDesc "Delete author"))
    <> command "delete-city" (info (cmdDeleteCity <**> helper) (progDesc "Delete city"))
    <> command "delete-publisher" (info (cmdDeletePublisher <**> helper) (progDesc "Delete publisher"))
    <> command "delete-conference" (info (cmdDeleteConference <**> helper) (progDesc "Delete conference"))
    <> command "delete-journal" (info (cmdDeleteJournal <**> helper) (progDesc "Delete journal"))
    <> command "delete-book" (info (cmdDeleteBook <**> helper) (progDesc "Delete book"))
    <> command "delete-book-author" (info (cmdDeleteBookAuthor <**> helper) (progDesc "Delete book-author pair"))
    <> command "delete-article" (info (cmdDeleteArticle <**> helper) (progDesc "Delete article"))
    <> command "delete-article-author" (info (cmdDeleteArticleAuthor <**> helper) (progDesc "Delete article-author pair"))
    <> command "delete-thesis" (info (cmdDeleteThesis <**> helper) (progDesc "Delete thesis"))
    <> command "delete-thesis-author" (info (cmdDeleteThesisAuthor <**> helper) (progDesc "Delete thesis-author pair"))

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

executeCommand (CreateAuthor name) connection = queryCreateAuthor connection name
executeCommand (CreateCity name) connection = queryCreateCity connection name
executeCommand (CreatePublisher name) connection = queryCreatePublisher connection name
executeCommand (CreateConference name) connection = queryCreateConference connection name
executeCommand (CreateJournal name) connection = queryCreateJournal connection name
executeCommand (CreateBook title cId pId year) connection = queryCreateBook connection title cId pId year
executeCommand (CreateBookAuthor bId aId) connection = queryCreateBookAuthor connection bId aId
executeCommand (CreateArticle aTitle aJournalId aIssue aYear aPagesStart aPagesEnd) connection
  = queryCreateArticle connection aTitle aJournalId aIssue aYear aPagesStart aPagesEnd
executeCommand (CreateArticleAuthor arId aId) connection = queryCreateArticleAuthor connection arId aId
executeCommand (CreateThesis tTitle tCityId tConferenceId tYear tPagesStart tPagesEnd) connection
  = queryCreateThesis connection tTitle tCityId tConferenceId tYear tPagesStart tPagesEnd
executeCommand (CreateThesisAuthor tId aId) connection = queryCreateThesisAuthor connection tId aId

executeCommand (DeleteAuthor aId) connection = queryDeleteAuthor connection aId
executeCommand (DeleteCity cId) connection = queryDeleteCity connection cId
executeCommand (DeletePublisher pId) connection = queryDeletePublisher connection pId
executeCommand (DeleteConference cId) connection = queryDeleteConference connection cId
executeCommand (DeleteJournal jId) connection = queryDeleteJournal connection jId
executeCommand (DeleteBook bId) connection = queryDeleteBook connection bId
executeCommand (DeleteBookAuthor bId aId) connection = queryDeleteBookAuthor connection bId aId
executeCommand (DeleteArticle arId) connection = queryDeleteArticle connection arId
executeCommand (DeleteArticleAuthor arId aId) connection = queryDeleteArticleAuthor connection arId aId
executeCommand (DeleteThesis tId) connection = queryDeleteThesis connection tId
executeCommand (DeleteThesisAuthor tId aId) connection = queryDeleteThesisAuthor connection tId aId















