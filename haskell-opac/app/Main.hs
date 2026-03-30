module Main where

import Control.Monad.IO.Class (liftIO)
import DB.Connection (withConnection)
import DB.Migration (runMigrations)
import App.Env (Env(..), AppM, runAppM)
import CLI.Repl (startRepl)

main :: IO ()
main = withConnection "opac.db" $ \conn -> do
  runMigrations conn
  let env = Env
        { envConnection = conn
        , envDBPath     = "opac.db"
        }
  runAppM env startRepl