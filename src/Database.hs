{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs,ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
-- jup Persist needs quite some things
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Database
  ( migrateAll
  , runStream
  ) where

import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as MF

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Control.Monad.Logger (NoLoggingT)

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Aeson (ToJSON(..),FromJSON(..))
import qualified Data.Aeson as Json
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Int (Int64)
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Text (Text)
import           Data.Time (UTCTime, getCurrentTime)

import           Database.Persist.TH (share, persistLowerCase
                                     , mkPersist, mkMigrate, sqlSettings)
import           Database.Persist.Sql (runMigration, SqlBackend, (==.), (>=.))
import qualified Database.Persist.Sql as Sql

import           GHC.Generics

import           EventSourcing
import           Database.Model

----------------------------------------------------------------------
-- interpret using a Persistent-Sqlite database

type SqlStore ev = ReaderT SqlBackend (NoLoggingT (ResourceT IO))


runStream :: (ToJSON ev, FromJSON ev) => EventStream ev res -> SqlStore ev res
runStream query = do
  time <- liftIO getCurrentTime
  MF.iterM (interpretPersistent time) query


----------------------------------------------------------------------
-- interpreter

interpretPersistent
  :: (ToJSON ev, FromJSON ev) =>
     UTCTime -> EventStreamF ev (SqlStore ev res) -> SqlStore ev res
     
interpretPersistent time (NewAggregate cont) = do
  id <- next . map (dbEventAggId . Sql.entityVal)
        <$>  Sql.selectList [] [Sql.Desc DbEventAggId, Sql.LimitTo 1]
  Sql.insert (DbEvent id "" time) -- insert an empty entry to reserve the key
  cont id
  where
    next [] = 1
    next (nr:_) = nr + 1
    
interpretPersistent time (AddEvent id ev query) = do
  Sql.insert $ DbEvent id (LBS.toStrict $ Json.encode ev) time
  query
  
interpretPersistent time (GetEvents lowerBound cont) = do
  events <- mapMaybe rowToEv <$> Sql.selectList filters []
  cont events
  where
    filters =
      case lowerBound of
        None -> []
        StartAt number -> [DbEventId >=. Sql.toSqlKey number]
        
interpretPersistent time (Project key (Projection init fold final)) = do
  events <- mapMaybe rowToEv <$> Sql.selectList [DbEventAggId ==. key] []
  final $ foldr (flip fold) (init key) events



rowToEv row =
  let entity = Sql.entityVal row
      event = decodeJson $ dbEventJson entity
      rowNumber = Sql.fromSqlKey $ Sql.entityKey row
      rowAggregateId = dbEventAggId entity
      time = dbEventTimestamp entity
      metadata = Metadata time rowNumber rowAggregateId
  in fmap (\ev -> Event ev metadata) event
  where
    decodeJson = Json.decode . LBS.fromStrict
