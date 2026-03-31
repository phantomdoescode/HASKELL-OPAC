module Models.Book where

import Data.Text (Text)
import Database.SQLite.Simple (FromRow(..), field)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (toField)

data Book = Book
  { bookID :: Int,
    bookTitle :: Text,
    bookAuthor :: Text,
    bookPublication :: Text,
    bookISBN :: Text,
    bookGenre :: Text
  }
  deriving (Show, Eq)

instance FromRow Book where
  fromRow = Book
    <$> field  -- book_id
    <*> field  -- title
    <*> field  -- author
    <*> field  -- publication
    <*> field  -- isbn
    <*> field  -- genre

instance ToRow Book where
  toRow b =
    [ toField (bookTitle           b)
    , toField (bookAuthor          b)
    , toField (bookPublication     b)
    , toField (bookISBN            b)
    , toField (bookGenre           b)
    ]