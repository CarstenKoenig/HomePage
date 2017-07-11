{-# LANGUAGE ExistentialQuantification #-}
module EventSourcing.Projections
  ( Projection(..)
  , AggregateKey
  , Metadata (..)
  , projectEvents
  , liftP
  , lastP, collectP, listP
  , lastMetadataP, lastChangeP, lastEventnumberP
  )
where

import Control.Applicative ((<|>))
import Control.Arrow ((***))

import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Time (UTCTime)

import EventSourcing.Events (Embedding(extract), AggregateKey, Metadata(..), Event(..))


data Projection ev a =
  forall state . Projection
  { initState :: AggregateKey -> state
  , fold :: state -> Event ev -> state
  , final :: state -> a
  }
  

instance Functor (Projection ev) where
  fmap f (Projection i fd fi) =
    Projection i fd (f . fi)


instance Applicative (Projection ev) where
  pure a = Projection (const ()) const (const a)
  pF <*> pX = uncurry ($) <$> zipP pF pX


projectEvents :: Foldable f => Projection ev a -> AggregateKey -> f (Event ev) -> a
projectEvents (Projection init fold final) key =
  final . foldl' fold (init key)


zipP :: Projection ev a -> Projection ev b -> Projection ev (a,b)
zipP (Projection ia fda fia) (Projection ib fdb fib) =
  Projection (\key -> (ia key, ib key)) fold (fia *** fib)
  where
    fold (sa,sb) ev = (fda sa ev, fdb sb ev)


listP :: (ev -> a) -> Projection ev [a]
listP proj = Projection (const []) update reverse
  where
    update xs ev =
      proj (content ev) : xs


collectP :: (ev -> Maybe a) -> Projection ev [a]
collectP selector =
  catMaybes <$> listP selector


lastP :: (ev -> Maybe a) -> Projection ev (Maybe a)
lastP selector =
  Projection (const Nothing) fold id
  where
    fold found ev =
      selector (content ev) <|> found


lastMetadataP :: Projection ev (Maybe Metadata)
lastMetadataP = Projection (const Nothing) fold id
  where fold found ev = Just (metadata ev) <|> found


lastChangeP :: Projection ev (Maybe UTCTime)
lastChangeP = fmap timestamp <$> lastMetadataP


lastEventnumberP :: Projection ev (Maybe Int64)
lastEventnumberP = fmap eventNumber <$> lastMetadataP


liftP :: Embedding ev1 ev0 => Projection ev0 r -> Projection ev1 r
liftP (Projection init0 fold0 final0) =
  let fold1 state ev1 =
        case traverse extract ev1 of
          Just ev0 -> fold0 state ev0
          Nothing  -> state
  in Projection init0 fold1 final0
