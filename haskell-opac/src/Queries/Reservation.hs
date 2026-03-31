{-# LANGUAGE OverloadedStrings #-}

module Queries.Reservation
  ( insertReservation
  , getReservationsByUser
  , getReservationByID
  , getReservationByUserAndBook
  , getNextReservationForBook
  , deleteReservation
  ) where

import Control.Monad.Reader (asks, liftIO)
import Data.Time (Day)
import Database.SQLite.Simple (Only(..), execute, query)
import App.Env (AppM, envConnection)
import Models.Reservation (Reservation(..))

insertReservation :: Int -> Int -> Day -> AppM ()
insertReservation uid bid date = do
  conn <- asks envConnection
  liftIO $
    execute
      conn
      "INSERT INTO reservations (user_id, book_id, reservation_date) VALUES (?, ?, ?)"
      (uid, bid, date)

getReservationsByUser :: Int -> AppM [Reservation]
getReservationsByUser uid = do
  conn <- asks envConnection
  liftIO $
    query
      conn
      "SELECT * FROM reservations WHERE user_id = ?"
      (Only uid)

getReservationByID :: Int -> AppM (Maybe Reservation)
getReservationByID rid = do
  conn <- asks envConnection
  rows <- liftIO $ query conn
    "SELECT * FROM reservations WHERE reservation_id = ?"
    (Only rid)
  case rows of
    [] -> return Nothing
    r : _ -> return (Just r)

getReservationByUserAndBook :: Int -> Int -> AppM (Maybe Reservation)
getReservationByUserAndBook uid bid = do
  conn <- asks envConnection
  rows <- liftIO $ query conn
    "SELECT * FROM reservations WHERE user_id = ? AND book_id = ?"
    (uid, bid)
  case rows of
    [] -> return Nothing
    r : _ -> return (Just r)

getNextReservationForBook :: Int -> AppM (Maybe Reservation)
getNextReservationForBook bid = do
  conn <- asks envConnection
  rows <- liftIO $ query conn
    "SELECT * FROM reservations WHERE book_id = ? ORDER BY reservation_date ASC LIMIT 1"
    (Only bid)
  case rows of
    [] -> return Nothing
    r : _ -> return (Just r)

deleteReservation :: Int -> AppM ()
deleteReservation rid = do
  conn <- asks envConnection
  liftIO $ execute conn
    "DELETE FROM reservations WHERE reservation_id = ?"
    (Only rid)
