{-# LANGUAGE OverloadedStrings #-}

module Queries.Book
  ( insertBook
  , getBookByID
  , getBookByISBN
  , searchBooks
  , getAllBooks
  , updateBookCopies
  , deleteBook
  ) where

import Control.Monad.Reader (asks, liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (query, query_, execute)
import Database.SQLite.Simple (Only(..))
import App.Env (AppM, envConnection)
import Models.Book (Book(..))

insertBook :: Book -> AppM ()
insertBook b = do
  conn <- asks envConnection
  liftIO $ execute conn
    "INSERT INTO books \
    \ (title, author, publication, isbn, genre, available_copies, total_copies) \
    \ VALUES (?, ?, ?, ?, ?, ?, ?)"
    b

getBookByID :: Int -> AppM (Maybe Book)
getBookByID bid = do
  conn <- asks envConnection
  rows <- liftIO $ query conn
    "SELECT * FROM books WHERE book_id = ?" (Only bid)
  case rows of
    []    -> return Nothing
    b : _ -> return (Just b)

getBookByISBN :: Text -> AppM (Maybe Book)
getBookByISBN isbn = do
  conn <- asks envConnection
  rows <- liftIO $ query conn
    "SELECT * FROM books WHERE isbn = ?" (Only isbn)
  case rows of
    []    -> return Nothing
    b : _ -> return (Just b)

searchBooks :: Text -> AppM [Book]
searchBooks term = do
  conn <- asks envConnection
  let pattern = "%" <> term <> "%"
  liftIO $ query conn
    "SELECT * FROM books WHERE title LIKE ? OR author LIKE ? OR genre LIKE ?"
    (pattern, pattern, pattern)

getAllBooks :: AppM [Book]
getAllBooks = do
  conn <- asks envConnection
  liftIO $ query_ conn "SELECT * FROM books"

updateBookCopies :: Int -> Int -> AppM ()
updateBookCopies bid available = do
  conn <- asks envConnection
  liftIO $ execute conn
    "UPDATE books SET available_copies = ? WHERE book_id = ?"
    (available, bid)

deleteBook :: Int -> AppM ()
deleteBook bid = do
  conn <- asks envConnection
  liftIO $ execute conn
    "DELETE FROM books WHERE book_id = ?" (Only bid)