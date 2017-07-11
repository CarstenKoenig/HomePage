{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs,ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
-- jup Persist needs quite some things
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Database.Model where

import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import           Data.Time (UTCTime)

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
