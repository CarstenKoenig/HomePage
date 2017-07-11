{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module EventSourcing.EventStore
  ( EventStream
  , EventStreamF (..)
  , LowerBound (..)
  , newAggregate, addEvent, project, getEvents
  ) where

import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as MF

import           Data.Int (Int64)
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Text (Text)

import EventSourcing.Events
import EventSourcing.Projections


type EventStream ev = Free (EventStreamF ev)

data LowerBound
  = None
  | StartAt Int64


-- | Functor-Interface for an event-stream
data EventStreamF ev a
  = NewAggregate (AggregateKey -> a)
  | AddEvent AggregateKey ev a
  | GetEvents LowerBound ([Event ev] -> a)
  | Project AggregateKey (Projection ev a)
  deriving Functor



----------------------------------------------------------------------
-- the DSL

newAggregate :: EventStream ev AggregateKey
newAggregate = MF.liftF $ NewAggregate id


addEvent :: Embedding ev1 ev0 => AggregateKey -> ev0 -> EventStream ev1 ()
addEvent key event = MF.liftF $ AddEvent key (embed event) ()


project :: Embedding ev1 ev0 => AggregateKey -> Projection ev0 res -> EventStream ev1 res
project key = MF.liftF . Project key . liftP


getEvents :: Embedding ev1 ev0 => LowerBound -> EventStream ev1 [Event ev0]
getEvents bound = mapMaybe extractEvent <$> (MF.liftF $ GetEvents bound id)
