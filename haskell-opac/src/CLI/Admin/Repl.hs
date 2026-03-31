{-# LANGUAGE OverloadedStrings #-}

module CLI.Admin.Repl
  ( adminMenuLoop
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import App.Env (AppM)
import CLI.Display (printHeader)
import CLI.Prompt (selectFrom, ask)
import Queries.Book (insertBook, getAllBooks, deleteBook)
import Queries.Copy (insertCopy, getCopiesByBookID)
import Queries.AuditLog (getAllAuditLogs, insertAuditLog)
import Models.Book (Book(..))
import Models.Copy (Copy(..))

adminMenuLoop :: AppM ()
adminMenuLoop = do
  liftIO $ printHeader "ADMIN DASHBOARD"
  choice <- liftIO $ selectFrom "Select Admin Action:"
    [ "Catalog Management (Books)"
    , "Inventory Tracking (Copies)"
    , "View Audit Logs"
    , "Return to Main Menu"
    ]
  case choice of
    Just 1 -> catalogManagement >> adminMenuLoop
    Just 2 -> inventoryTracking >> adminMenuLoop
    Just 3 -> viewLogs >> adminMenuLoop
    Just 4 -> return ()
    _      -> adminMenuLoop

catalogManagement :: AppM ()
catalogManagement = do
  liftIO $ printHeader "Catalog Management"
  choice <- liftIO $ selectFrom "Actions:"
    [ "Add New Book Title"
    , "Delete Book Title"
    , "View All Titles"
    , "Back"
    ]
  case choice of
    Just 1 -> addBookAction >> catalogManagement
    Just 2 -> deleteBookAction >> catalogManagement
    Just 3 -> viewAllBooksAction >> catalogManagement
    _      -> return ()

addBookAction :: AppM ()
addBookAction = do
  title <- liftIO $ ask "Enter Title:"
  author <- liftIO $ ask "Enter Author:"
  pub <- liftIO $ ask "Enter Publication:"
  isbn <- liftIO $ ask "Enter ISBN:"
  genre <- liftIO $ ask "Enter Genre:"
  let book = Book 0 title author pub isbn genre
  insertBook book
  insertAuditLog Nothing ("Added Book Title: " <> title)
  liftIO $ putStrLn "Book title added successfully."

deleteBookAction :: AppM ()
deleteBookAction = do
  bidStr <- liftIO $ ask "Enter Book ID to Delete:"
  case reads (T.unpack bidStr) of
    [(bid, "")] -> do
      deleteBook bid
      insertAuditLog Nothing ("Deleted Book ID: " <> bidStr)
      liftIO $ putStrLn "Book title deleted."
    _ -> liftIO $ putStrLn "Invalid ID."

viewAllBooksAction :: AppM ()
viewAllBooksAction = do
  books <- getAllBooks
  liftIO $ mapM_ print books

inventoryTracking :: AppM ()
inventoryTracking = do
  liftIO $ printHeader "Inventory Tracking"
  bidStr <- liftIO $ ask "Enter Book ID to manage copies:"
  case reads (T.unpack bidStr) of
    [(bid, "")] -> do
      choice <- liftIO $ selectFrom "Actions:"
        [ "Add Copy"
        , "View Copies"
        , "Back"
        ]
      case choice of
        Just 1 -> addCopyAction bid >> inventoryTracking
        Just 2 -> viewCopiesAction bid >> inventoryTracking
        _      -> return ()
    _ -> liftIO $ putStrLn "Invalid ID."

addCopyAction :: Int -> AppM ()
addCopyAction bid = do
  ctrlNum <- liftIO $ ask "Enter Control Number (Barcode):"
  let copy = Copy 0 bid ctrlNum "Available"
  insertCopy copy
  insertAuditLog Nothing ("Added Copy: " <> ctrlNum <> " for Book ID: " <> T.pack (show bid))
  liftIO $ putStrLn "Copy added successfully."

viewCopiesAction :: Int -> AppM ()
viewCopiesAction bid = do
  copies <- getCopiesByBookID bid
  liftIO $ mapM_ print copies

viewLogs :: AppM ()
viewLogs = do
  liftIO $ printHeader "Audit Logs"
  logs <- getAllAuditLogs
  liftIO $ mapM_ print logs
