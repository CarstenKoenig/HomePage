{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs,ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
-- jup Persist needs quite some things
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Database.Model where

import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)

import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import           Data.Time (UTCTime)

import           Data.Pool (Pool)

import           Database.Persist.Sql (SqlBackend, runSqlPool)
import           Database.Persist.TH (share, persistLowerCase
                                     , mkPersist, mkMigrate, sqlSettings)

import           EventSourcing

----------------------------------------------------------------------
-- interpret using a Persistent-Sqlite database

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbEvent
    aggId Int64
    json ByteString
    timestamp UTCTime
    deriving Show
|]


type SqlQuery res = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) res
  

runSqlQuery :: Pool SqlBackend -> SqlQuery res -> IO res
runSqlQuery pool query =
  runResourceT . runNoLoggingT $
    runSqlPool query pool
