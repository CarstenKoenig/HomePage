{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs,ExistentialQuantification #-}

module Database
  ( SqlQuery
  , runSqlQuery
  , runStream
  , migrateAll
  ) where

import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Pool (Pool)

import           Database.Persist.Sql (SqlBackend, runSqlPool)

import           Database.EventSourcing
import           Database.Model


runSqlQuery :: Pool SqlBackend -> SqlQuery res -> IO res
runSqlQuery pool query =
  runResourceT . runNoLoggingT $
    runSqlPool query pool
