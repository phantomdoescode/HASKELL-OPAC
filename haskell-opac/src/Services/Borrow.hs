{-# LANGUAGE OverloadedStrings #-}

module Services.Borrow
  ( borrowBook
  , returnBook
  , calculateFine
  ) where

import Control.Monad.Reader (ask, liftIO, runReaderT)
import Data.Time (Day, diffDays)
import App.Env (AppM, Env(..))
import App.Error (AppError(..))
import DB.Connection (withTransaction)
import Models.Book (Book(..))
import Models.BorrowRecord (BorrowRecord(..))
import Models.User (User(..))
import Models.Copy (Copy(..))
import Queries.Copy (getAvailableCopy, updateCopyStatus, getCopyByID)
import Queries.BorrowRecord (insertBorrowRecord, markAsReturned)
import Queries.User (getUserByID, updateUserFineBalance)
import Services.Notification (publishReturnEvent)

dailyRate :: Double
dailyRate = 5.0

fineCap :: Double
fineCap = 500.0

borrowBook :: User -> Book -> Day -> Day -> AppM (Either AppError ())
borrowBook user book bDate dueDate = do
  env <- ask
  mCopy <- getAvailableCopy (bookID book)
  case mCopy of
    Nothing -> return $ Left (ValidationError "No copies of this book are currently available.")
    Just copy -> liftIO $ withTransaction (envConnection env) $ do
      runReaderT (action copy) env
      return $ Right ()
  where
    action :: Copy -> AppM ()
    action copy = do
      insertBorrowRecord (borrowRecord copy)
      updateCopyStatus (copyID copy) "Borrowed"

    borrowRecord copy =
      BorrowRecord
        { borrowID = 0
        , borrowUserID = userID user
        , borrowCopyID = copyID copy
        , borrowDate = bDate
        , borrowDueDate = dueDate
        , borrowReturnDate = Nothing
        , borrowIsReturned = False
        }

returnBook :: BorrowRecord -> Day -> AppM (Either AppError ())
returnBook record returnDate = do
  env <- ask
  mUser <- getUserByID (borrowUserID record)
  mCopy <- getCopyByID (borrowCopyID record)
  case (mUser, mCopy) of
    (Just user, Just copy) -> liftIO $ withTransaction (envConnection env) $ do
      -- Mark record as returned
      runReaderT (markAsReturned (borrowID record) returnDate) env
      -- Update copy status
      runReaderT (updateCopyStatus (copyID copy) "Available") env
      -- Calculate and update fine
      let fine = calculateFine (borrowDueDate record) returnDate
      if fine > 0
        then runReaderT (updateUserFineBalance (userID user) (userFineBalance user + fine)) env
        else return ()
      -- Trigger notification for reservations
      runReaderT (publishReturnEvent (copyBookID copy)) env
      return $ Right ()
    _ -> return $ Left (DBError "Required records for returning book not found.")

calculateFine :: Day -> Day -> Double
calculateFine dueDate returnDate =
  let l = max 0 (diffDays returnDate dueDate)
  in min (fromIntegral l * dailyRate) fineCap
