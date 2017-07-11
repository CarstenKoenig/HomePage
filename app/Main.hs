{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson (ToJSON(..),FromJSON(..))
import           Data.Text (Text)

import           Database.Persist.Sql (runMigration)
import           Database.Persist.Sqlite (runSqlite)


import Application

import EventSourcing
import Database

main :: IO ()
main = startApp


runInSqlite :: (ToJSON ev, FromJSON ev) => Text -> EventStream ev res -> IO res
runInSqlite connection query = do
  runSqlite connection $ do
    runMigration migrateAll
    runStream query


dbConnection :: Text
dbConnection = "events.db"

