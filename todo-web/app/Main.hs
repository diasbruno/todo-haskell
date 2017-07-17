{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Monoid (mconcat)
import           Data.Text
import           Data.Todo
import qualified Database.Persist.Sqlite as S
import           Database.Persist.Sqlite hiding (delete)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

db = liftIO . dbSession

jsonResp :: [Text] -> ActionM ()
jsonResp = json . mconcat

tpack = Data.Text.pack

authorNew :: ScottyM ()
authorNew = post "/author/new" $ do
  n <- param "name"
  db $ S.insert (Author n)
  jsonResp ["{", "\"status\":", "\"success\",", "\"author\":", tpack n, "}"]

authorDelete :: ScottyM ()
authorDelete = post "/author/delete" $ do
  n <- param "id"
  db $ S.delete (toSqlKey n :: Key Author)
  jsonResp ["{", "\"status\":", "\"success\"", "}"]

todoNew :: ScottyM ()
todoNew = post "/todo/new" $ do
  t <- param "title"
  n <- param "author"
  i <- db $ S.insert (Todo t (fromEnum ToDo) (toSqlKey n))
  jsonResp ["{", "\"status\":", "\"success\"", "\"todo\":", tpack (show i), "}"]

todoUpdateStatus :: ScottyM ()
todoUpdateStatus = post "/todo/update_status" $ do
  n <- param "id"
  s <- param "status"
  db $ S.update (toSqlKey n) [TodoStatus =. (toEnum $ read s)]
  jsonResp ["{", "\"status\":", "\"success\"", "}"]

todoDelete :: ScottyM ()
todoDelete = post "/todo/delete" $ do
  n <- param "id"
  db $ S.delete (toSqlKey n :: Key Author)
  jsonResp ["{", "\"status\":", "\"success\"", "}"]

server :: ScottyM () -> IO ()
server = scotty 3000

performMigration = dbSession $ do
    runMigration migrateAll

main :: IO ()
main = performMigration >>
  (server $ do
      middleware logStdoutDev
      authorNew
      authorDelete
      todoNew
      todoUpdateStatus
      todoDelete)
