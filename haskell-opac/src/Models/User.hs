module Models.User where

import Data.Text (Text)
import Data.Time (Day)
import Database.SQLite.Simple (FromRow (..), field)
import Database.SQLite.Simple.ToField (toField)
import Database.SQLite.Simple.ToRow (ToRow (..))

data User = User
  { userID :: Int,
    userPasswordHash :: Text,
    userFirstName :: Text,
    userLastName :: Text,
    userEmail :: Text,
    userBirthDate :: Day,
    userOccupation :: Text,
    userOrganization :: Text,
    userFineBalance :: Double,
    userType :: Text
  }
  deriving (Show, Eq)

instance FromRow User where
  fromRow =
    User
      <$> field -- user_id
      <*> field -- password_hash
      <*> field -- first_name
      <*> field -- last_name
      <*> field -- email
      <*> field -- birth_date
      <*> field -- occupation
      <*> field -- organization
      <*> field -- fine_balance
      <*> field -- user_type

instance ToRow User where
  toRow u =
    [ toField (userPasswordHash u),
      toField (userFirstName u),
      toField (userLastName u),
      toField (userEmail u),
      toField (userBirthDate u),
      toField (userOccupation u),
      toField (userOrganization u),
      toField (userFineBalance u),
      toField (userType u)
    ]