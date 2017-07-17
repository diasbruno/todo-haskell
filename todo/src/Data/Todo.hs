{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Data.Todo where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int (Int64)

data ToDoStatus = ToDo | OnProgress | Done deriving (Show, Read, Eq, Enum)
derivePersistField "ToDoStatus"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name String
    deriving Show
Todo
    title String
    status Int default=0
    authorId AuthorId
    deriving Show
|]

-- | dbSession create a new connection to the sqlite and performs a query.
-- NOTE: NoLoggingT (ResourceT IO) = runResourceT . runNoLoggingT
dbSession :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
dbSession = runResourceT . runNoLoggingT . withSqliteConn "todo.db" . runSqlConn
