{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
module EventSourcing.Events
  ( AggregateKey
  , Metadata (..)
  , Event (..)
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
    
