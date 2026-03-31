{-# LANGUAGE OverloadedStrings #-}

module CLI.Repl
  ( startRepl
  ) where

import Control.Monad.IO.Class (liftIO)
import App.Env (AppM)
import CLI.Display (printHeader)
import CLI.Prompt (selectFrom)
import CLI.User.Auth (registerScreen, loginScreen)
import CLI.User.Actions (userMenuLoop)
import CLI.Admin.Repl (adminMenuLoop)
import Models.User (User(..))

startRepl :: AppM ()
startRepl = mainMenuLoop

mainMenuLoop :: AppM ()
mainMenuLoop = do
  liftIO $ printHeader "Welcome to OPAC System"
  choice <- liftIO $ selectFrom "Please select an option:"
    [ "Register Account"
    , "Sign In"
    , "Exit"
    ]
  case choice of
    Just 1  -> do
      registerScreen
      mainMenuLoop
    Just 2  -> do
      mUser <- loginScreen
      case mUser of
        Nothing   -> mainMenuLoop
        Just user -> do
          if userType user == "admin"
            then adminMenuLoop
            else userMenuLoop user
          mainMenuLoop
    Just 3  -> liftIO $ putStrLn "Goodbye!"
    _       -> mainMenuLoop