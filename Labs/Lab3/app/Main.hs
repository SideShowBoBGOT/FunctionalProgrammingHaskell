module Main (main) where

--import Database.PostgreSQL.Simple
import Options.Applicative
import Data.Semigroup ((<>))
import Lib

--data Author = Author { authorName :: String }
--
--data Book = Book {
--    bookAuthors :: [Author],
--    bookTitle :: String,
--    bookCity :: String,
--    bookPublisher :: String,
--    bookYear :: Int
--}
--
--data Article = Article {
--    articleAuthors :: [Author],
--    articleTitle :: String,
--    articleJournal :: String,
--    articleYear :: Int,
--    articleIssue :: Int,
--    articlePages :: (Int, Int)
--}
--
--data Thesis = Thesis {
--    thesisAuthors :: [Author],
--    thesisTitle :: String,
--    thesisConference :: String,
--    thesisCity :: String,
--    thesisYear :: Int,
--    thesisPages :: (Int, Int)
--}

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
  command <- execParser programParser
  print command