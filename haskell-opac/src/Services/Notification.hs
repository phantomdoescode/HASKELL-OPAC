{-# LANGUAGE OverloadedStrings #-}

module Services.Notification
  ( publishReturnEvent
  , startNotificationWorker
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.Reader (ask, liftIO, runReaderT)
import App.Env (AppM, Env(..))
import Queries.Reservation (getNextReservationForBook, deleteReservation)
import Models.Reservation (Reservation(..))

-- | Publishes a return event to the channel for background processing
publishReturnEvent :: Int -> AppM ()
publishReturnEvent bid = do
  env <- ask
  liftIO $ writeChan (envReturnChan env) bid

-- | Starts a background worker to handle return events from the channel
startNotificationWorker :: Env -> IO ()
startNotificationWorker env = do
  _ <- forkIO $ forever $ do
    bid <- readChan (envReturnChan env)
    runReaderT (handleReturnEvent bid) env
  return ()

-- | Internal logic to process a return event: FIFO reservation check and alerting
handleReturnEvent :: Int -> AppM ()
handleReturnEvent bid = do
  mRes <- getNextReservationForBook bid
  case mRes of
    Nothing -> liftIO $ putStrLn $ "[Worker] No reservations pending for Book ID: " ++ show bid
    Just res -> do
      liftIO $ putStrLn $ "[Worker] ALERT: FIFO Reservation triggered. Next user: " 
                  ++ show (reservationUserID res) ++ " notified for Book " ++ show bid
      -- Clear the reservation as it's now 'served' by the return
      deleteReservation (reservationID res)
