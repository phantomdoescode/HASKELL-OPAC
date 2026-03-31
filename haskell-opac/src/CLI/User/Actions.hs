{-# LANGUAGE OverloadedStrings #-}

module CLI.User.Actions
  ( userMenuLoop
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addDays, getCurrentTime, utctDay)
import App.Env (AppM)
import App.Error (AppError(..))
import Models.Book (Book(..))
import Models.BorrowRecord (BorrowRecord(..))
import Models.Reservation (Reservation(..))
import Models.User (User(..))
import Models.Copy (Copy(..))
import Queries.Book (searchBooks, getAllBooks, getBookByID)
import Queries.Copy (getCopyByID)
import Queries.BorrowRecord (getActiveBorrowsByUser)
import Services.Borrow (borrowBook, returnBook)
import Services.Profile (changePassword, updateProfile)
import Services.Reservation
  ( cancelReservation
  , getReservationsForUser
  , reserveBook
  )
import CLI.Display
  ( printBook
  , printError
  , printHeader
  , printInfo
  , printSuccess
  , printUser
  )
import CLI.Prompt (ask, askHidden, confirm, selectFrom)

userMenuLoop :: User -> AppM ()
userMenuLoop user = do
  liftIO $ printHeader $ "Hello, " <> userFirstName user
  choice <- liftIO $ selectFrom "What would you like to do?"
    [ "Search Books"
    , "Borrow / Return"
    , "My Reservations"
    , "My Profile"
    , "Logout"
    ]
  case choice of
    Just 1 -> searchScreen >> userMenuLoop user
    Just 2 -> borrowReturnScreen user >> userMenuLoop user
    Just 3 -> reservationsScreen user >> userMenuLoop user
    Just 4 -> do
      updatedUser <- profileScreen user
      userMenuLoop updatedUser
    Just 5 -> return ()
    _ -> userMenuLoop user

searchScreen :: AppM ()
searchScreen = do
  liftIO $ printHeader "Search Books"
  query <- liftIO $ ask "Enter title, author, or genre (leave blank for all)"
  books <- if T.null (T.strip query)
    then getAllBooks
    else searchBooks query
  if null books
    then liftIO $ printInfo "No books were found."
    else liftIO $ mapM_ printBook books

borrowReturnScreen :: User -> AppM ()
borrowReturnScreen user = do
  liftIO $ printHeader "Borrow / Return"
  choice <- liftIO $ selectFrom "Choose an action:"
    [ "Borrow a book"
    , "Return a book"
    , "Back"
    ]
  case choice of
    Just 1 -> borrowBookFlow user >> borrowReturnScreen user
    Just 2 -> returnBookFlow user >> borrowReturnScreen user
    Just 3 -> return ()
    _ -> borrowReturnScreen user

borrowBookFlow :: User -> AppM ()
borrowBookFlow user = do
  books <- getAllBooks
  if null books
    then liftIO $ printInfo "There are no books in the catalog."
    else do
      maybeBook <- liftIO $ selectBook books
      case maybeBook of
        Nothing -> liftIO $ printInfo "No book selected."
        Just book -> do
          confirmBorrow <- liftIO $ confirm $ "Borrow '" <> bookTitle book <> "'?"
          if not confirmBorrow
            then liftIO $ printInfo "Borrow cancelled."
            else do
              today <- liftIO $ utctDay <$> getCurrentTime
              let dueDate = addDays 14 today
              result <- borrowBook user book today dueDate
              case result of
                Left err -> liftIO $ printError $ formatAppError err
                Right () -> liftIO $ printSuccess $ "Borrowed '" <> bookTitle book <> "' until " <> T.pack (show dueDate) <> "."

returnBookFlow :: User -> AppM ()
returnBookFlow user = do
  records <- getActiveBorrowsByUser (userID user)
  if null records
    then liftIO $ printInfo "You have no active borrowed books."
    else do
      maybeRecord <- selectBorrowRecord records
      case maybeRecord of
        Nothing -> liftIO $ printInfo "No borrow record selected."
        Just record -> do
          confirmReturn <- liftIO $ confirm "Return the selected book?"
          if not confirmReturn
            then liftIO $ printInfo "Return cancelled."
            else do
              today <- liftIO $ utctDay <$> getCurrentTime
              result <- returnBook record today
              case result of
                Left err -> liftIO $ printError $ formatAppError err
                Right () -> liftIO $ printSuccess "Book return completed."

reservationsScreen :: User -> AppM ()
reservationsScreen user = do
  liftIO $ printHeader "Reservations"
  choice <- liftIO $ selectFrom "Choose an action:"
    [ "View my reservations"
    , "Add a reservation"
    , "Cancel a reservation"
    , "Back"
    ]
  case choice of
    Just 1 -> viewReservationsFlow user >> reservationsScreen user
    Just 2 -> addReservationFlow user >> reservationsScreen user
    Just 3 -> cancelReservationFlow user >> reservationsScreen user
    Just 4 -> return ()
    _ -> reservationsScreen user

viewReservationsFlow :: User -> AppM ()
viewReservationsFlow user = do
  reservations <- getReservationsForUser (userID user)
  if null reservations
    then liftIO $ printInfo "You have no reservations."
    else mapM_ displayReservation reservations

displayReservation :: Reservation -> AppM ()
displayReservation reservation = do
  mbBook <- getBookByID (reservationBookID reservation)
  case mbBook of
    Nothing -> liftIO $ printError "Reserved book not found."
    Just book -> do
      liftIO $ printBook book
      liftIO $ printInfo $ "Reserved on: " <> T.pack (show $ reservationDate reservation)

addReservationFlow :: User -> AppM ()
addReservationFlow user = do
  books <- getAllBooks
  if null books
    then liftIO $ printInfo "No books are available to reserve."
    else do
      maybeBook <- liftIO $ selectBook books
      case maybeBook of
        Nothing -> liftIO $ printInfo "No book selected."
        Just book -> do
          confirmReserve <- liftIO $ confirm $ "Reserve '" <> bookTitle book <> "'?"
          if not confirmReserve
            then liftIO $ printInfo "Reservation cancelled."
            else do
              today <- liftIO $ utctDay <$> getCurrentTime
              result <- reserveBook user book today
              case result of
                Left err -> liftIO $ printError $ formatAppError err
                Right () -> liftIO $ printSuccess $ "Reserved '" <> bookTitle book <> "'."

cancelReservationFlow :: User -> AppM ()
cancelReservationFlow user = do
  reservations <- getReservationsForUser (userID user)
  if null reservations
    then liftIO $ printInfo "You have no reservations to cancel."
    else do
      maybeReservation <- selectReservation reservations
      case maybeReservation of
        Nothing -> liftIO $ printInfo "No reservation selected."
        Just reservation -> do
          confirmCancel <- liftIO $ confirm "Cancel this reservation?"
          if not confirmCancel
            then liftIO $ printInfo "Cancellation aborted."
            else do
              result <- cancelReservation (reservationID reservation)
              case result of
                Left err -> liftIO $ printError $ formatAppError err
                Right () -> liftIO $ printSuccess "Reservation cancelled."

profileScreen :: User -> AppM User
profileScreen user = do
  liftIO $ printHeader "My Profile"
  choice <- liftIO $ selectFrom "Choose an action:"
    [ "View profile"
    , "Edit profile"
    , "Change password"
    , "Back"
    ]
  case choice of
    Just 1 -> viewProfile user >> profileScreen user
    Just 2 -> editProfileFlow user
    Just 3 -> changePasswordFlow user
    Just 4 -> return user
    _ -> profileScreen user

viewProfile :: User -> AppM ()
viewProfile user = do
  liftIO $ printUser user
  liftIO $ printInfo $ "Birthdate: " <> T.pack (show $ userBirthDate user)
  liftIO $ printInfo $ "Fine Balance: $" <> T.pack (show $ userFineBalance user)

editProfileFlow :: User -> AppM User
editProfileFlow user = do
  firstName <- liftIO $ ask $ "First Name [" <> userFirstName user <> "]"
  lastName <- liftIO $ ask $ "Last Name [" <> userLastName user <> "]"
  occupation <- liftIO $ ask $ "Occupation [" <> userOccupation user <> "]"
  organization <- liftIO $ ask $ "Organization [" <> userOrganization user <> "]"
  let newFirst = nonEmptyOrDefault firstName (userFirstName user)
      newLast = nonEmptyOrDefault lastName (userLastName user)
      newOccupation = nonEmptyOrDefault occupation (userOccupation user)
      newOrganization = nonEmptyOrDefault organization (userOrganization user)
  result <- updateProfile user newFirst newLast newOccupation newOrganization
  case result of
    Left err -> do
      liftIO $ printError $ formatAppError err
      return user
    Right updated -> do
      liftIO $ printSuccess "Profile updated successfully."
      return updated

changePasswordFlow :: User -> AppM User
changePasswordFlow user = do
  currentPassword <- liftIO $ askHidden "Current password"
  newPassword <- liftIO $ askHidden "New password"
  confirmPassword <- liftIO $ askHidden "Confirm new password"
  if newPassword /= confirmPassword
    then do
      liftIO $ printError "New passwords do not match."
      return user
    else do
      result <- changePassword user currentPassword newPassword
      case result of
        Left err -> do
          liftIO $ printError $ formatAppError err
          return user
        Right updated -> do
          liftIO $ printSuccess "Password changed successfully."
          return updated

selectBook :: [Book] -> IO (Maybe Book)
selectBook books = do
  choice <- selectFrom "Select a book:" (map formatBook books)
  case choice of
    Just n | n >= 1 && n <= length books -> return $ Just (books !! (n - 1))
    _ -> return Nothing
  where
    formatBook book =
      T.pack (show $ bookID book)
        <> " - "
        <> bookTitle book
        <> " by "
        <> bookAuthor book

selectBorrowRecord :: [BorrowRecord] -> AppM (Maybe BorrowRecord)
selectBorrowRecord records = do
  options <- forM records $ \record -> do
    mCopy <- getCopyByID (borrowCopyID record)
    case mCopy of
      Nothing -> return ("Unknown Copy " <> T.pack (show $ borrowCopyID record), record)
      Just copy -> do
        mbBook <- getBookByID (copyBookID copy)
        let title = maybe ("Book ID " <> T.pack (show $ copyBookID copy)) bookTitle mbBook
        return
          ( T.pack (show $ borrowID record)
              <> " - "
              <> title
              <> " (due "
              <> T.pack (show $ borrowDueDate record)
              <> ")",
            record
          )
  choice <- liftIO $ selectFrom "Select a borrow record to return:" (map fst options)
  case choice of
    Just n | n >= 1 && n <= length options -> return $ Just (snd $ options !! (n - 1))
    _ -> return Nothing

selectReservation :: [Reservation] -> AppM (Maybe Reservation)
selectReservation reservations = do
  options <- forM reservations $ \reservation -> do
    mbBook <- getBookByID (reservationBookID reservation)
    let title = maybe ("Book " <> T.pack (show $ reservationBookID reservation)) bookTitle mbBook
    return
      ( T.pack (show $ reservationID reservation)
          <> " - "
          <> title
          <> " (reserved "
          <> T.pack (show $ reservationDate reservation)
          <> ")",
        reservation
      )
  choice <- liftIO $ selectFrom "Select a reservation to cancel:" (map fst options)
  case choice of
    Just n | n >= 1 && n <= length options -> return $ Just (snd $ options !! (n - 1))
    _ -> return Nothing

nonEmptyOrDefault :: Text -> Text -> Text
nonEmptyOrDefault input def = if T.null (T.strip input) then def else input

formatAppError :: AppError -> Text
formatAppError (NotFound msg) = T.pack msg
formatAppError (AlreadyExists msg) = T.pack msg
formatAppError (AuthError msg) = T.pack msg
formatAppError (DBError msg) = T.pack msg
formatAppError (ValidationError msg) = T.pack msg
