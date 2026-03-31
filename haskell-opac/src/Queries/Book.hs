{-# LANGUAGE OverloadedStrings #-}

module Queries.Book
  ( insertBook
  , getBookByID
  , getBookByISBN
  , searchBooks
  , getAllBooks
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
    \ (title, author, publication, isbn, genre) \
    \ VALUES (?, ?, ?, ?, ?)"
    (bookTitle b, bookAuthor b, bookPublication b, bookISBN b, bookGenre b)

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

deleteBook :: Int -> AppM ()
deleteBook bid = do
  conn <- asks envConnection
  liftIO $ execute conn
    "DELETE FROM books WHERE book_id = ?" (Only bid)