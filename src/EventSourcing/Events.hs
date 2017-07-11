{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module EventSourcing.Events
  ( AggregateKey
  , Metadata (..)
  , Event (..)
  , Embedding (..)
  , embeddEvent, extractEvent
  )
where

import Data.Int (Int64)
import Data.Time (UTCTime)

type AggregateKey = Int64

data Metadata =
  Metadata
  { timestamp :: UTCTime
  , eventNumber :: Int64
  , aggregateKey :: AggregateKey
  }
  deriving (Eq, Show)


data Event ev =
  Event
  { content :: ev
  , metadata :: Metadata
  }
  deriving (Eq, Show, Functor)


instance Foldable Event where
  foldMap map = map . content


instance Traversable Event where
  traverse map ev =
    let wrap x = Event x (metadata ev)
    in fmap wrap (map $ content ev)
    

class Embedding sup sub where
  embed :: sub -> sup
  extract :: sup -> Maybe sub


instance Embedding a a where
  embed = id
  extract = Just . id


embeddEvent :: Embedding sup sub => Event sub -> Event sup
embeddEvent = fmap embed


extractEvent :: Embedding sup sub => Event sup -> Maybe (Event sub)
extractEvent = traverse extract
