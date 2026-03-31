module Models.Copy where

import Data.Text (Text)
import Database.SQLite.Simple (FromRow(..), field)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (toField)

data CopyStatus = Available | Borrowed | Reserved | Maintenance
  deriving (Show, Eq, Read)

data Copy = Copy
  { copyID            :: Int
  , copyBookID        :: Int
  , copyControlNumber :: Text
  , copyStatus        :: Text -- Store as text in SQLite or use a mapper
  } deriving (Show, Eq)

instance FromRow Copy where
  fromRow = Copy
    <$> field -- copy_id
    <*> field -- book_id
    <*> field -- control_number
    <*> field -- status

instance ToRow Copy where
  toRow c =
    [ toField (copyBookID        c)
    , toField (copyControlNumber c)
    , toField (copyStatus        c)
    ]
