{-# LANGUAGE OverloadedStrings #-}

module DB.Migration
  ( runMigrations,
  )
where

import DB.Schema
  ( createAuditLogsTable,
    createBooksTable,
    createBorrowRecordsTable,
    createCopiesTable,
    createReservationsTable,
    createUsersTable,
  )
import Data.String (fromString)
import Database.SQLite.Simple (Connection, Only (..), execute_, query_)

createVersionTable :: Connection -> IO ()
createVersionTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS schema_version (\
    \ version INTEGER PRIMARY KEY\
    \)"

getSchemaVersion :: Connection -> IO Int
getSchemaVersion conn = do
  rows <- query_ conn "SELECT version FROM schema_version ORDER BY version DESC LIMIT 1"
  case rows of
    [] -> return 0
    (Only v) : _ -> return v

setSchemaVersion :: Connection -> Int -> IO ()
setSchemaVersion conn v =
  execute_
    conn
    ( "INSERT OR REPLACE INTO schema_version (version) VALUES ("
        <> fromString (show v)
        <> ")"
    )

migrations :: [(Int, [Connection -> IO ()])]
migrations =
  [ ( 1,
      [ \conn -> execute_ conn createUsersTable,
        \conn -> execute_ conn createBooksTable,
        \conn -> execute_ conn createBorrowRecordsTable
      ]
    ),
    ( 2,
      [ \conn -> execute_ conn createReservationsTable
      ]
    ),
    ( 3,
      [ \conn -> execute_ conn "DROP TABLE IF EXISTS reservations",
        \conn -> execute_ conn "DROP TABLE IF EXISTS borrow_records",
        \conn -> execute_ conn "DROP TABLE IF EXISTS books",
        \conn -> execute_ conn "DROP TABLE IF EXISTS users",
        \conn -> execute_ conn "DROP TABLE IF EXISTS copies",
        \conn -> execute_ conn "DROP TABLE IF EXISTS audit_logs",
        \conn -> execute_ conn createUsersTable,
        \conn -> execute_ conn createBooksTable,
        \conn -> execute_ conn createCopiesTable,
        \conn -> execute_ conn createBorrowRecordsTable,
        \conn -> execute_ conn createReservationsTable,
        \conn -> execute_ conn createAuditLogsTable
      ]
    ),
    ( 4,
      [ \conn -> execute_ conn "DROP TABLE IF EXISTS reservations",
        \conn -> execute_ conn "DROP TABLE IF EXISTS borrow_records",
        \conn -> execute_ conn "DROP TABLE IF EXISTS books",
        \conn -> execute_ conn "DROP TABLE IF EXISTS users",
        \conn -> execute_ conn "DROP TABLE IF EXISTS copies",
        \conn -> execute_ conn "DROP TABLE IF EXISTS audit_logs",
        \conn -> execute_ conn createUsersTable,
        \conn -> execute_ conn createBooksTable,
        \conn -> execute_ conn createCopiesTable,
        \conn -> execute_ conn createBorrowRecordsTable,
        \conn -> execute_ conn createReservationsTable,
        \conn -> execute_ conn createAuditLogsTable
      ]
    )
  ]

runMigrations :: Connection -> IO ()
runMigrations conn = do
  createVersionTable conn
  currentVersion <- getSchemaVersion conn
  let pending = filter (\(v, _) -> v > currentVersion) migrations
  mapM_ (runMigration conn) pending

runMigration :: Connection -> (Int, [Connection -> IO ()]) -> IO ()
runMigration conn (v, steps) = do
  mapM_ ($ conn) steps
  setSchemaVersion conn v
  putStrLn $ "Migrated to version " <> show v