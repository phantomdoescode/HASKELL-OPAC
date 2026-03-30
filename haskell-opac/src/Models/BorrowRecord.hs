module Models.BorrowRecord
  ( BorrowRecord(..)
  , isOverdue
  ) where

import Data.Time (Day)
import Database.SQLite.Simple (FromRow(..), field)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (toField)

data BorrowRecord = BorrowRecord
  { borrowID         :: Int
  , borrowUserID     :: Int
  , borrowBookID     :: Int
  , borrowDate       :: Day
  , borrowDueDate    :: Day
  , borrowReturnDate :: Maybe Day
  , borrowIsReturned :: Bool
  } deriving (Show, Eq)

instance FromRow BorrowRecord where
  fromRow = BorrowRecord
    <$> field  -- borrow_id
    <*> field  -- user_id
    <*> field  -- book_id
    <*> field  -- borrow_date
    <*> field  -- due_date
    <*> field  -- return_date
    <*> field  -- is_returned

instance ToRow BorrowRecord where
  toRow r =
    [ toField (borrowUserID     r)
    , toField (borrowBookID     r)
    , toField (borrowDate       r)
    , toField (borrowDueDate    r)
    , toField (borrowReturnDate r)
    , toField (borrowIsReturned r)
    ]

isOverdue :: Day -> BorrowRecord -> Bool
isOverdue today r = not (borrowIsReturned r) && borrowDueDate r < today