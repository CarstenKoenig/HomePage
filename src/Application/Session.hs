-- REMARK:
-- everything here is totally inspired (copied) from https://www.stackbuilders.com/tutorials/haskell/servant-auth/
-- cudos for all those hints

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application.Session where

import Data.Serialize hiding (Get)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)


-- | A 'Session' here for now just includes the time when the session was created

data Session = Session
  { sessionStart :: UTCTime
  }


-- In order to store the 'Session' in a cookie, we need to make it an
-- instance of the 'Serialize' type class.

instance Serialize Session where
  put (Session time) =
    put (toRational $ utcTimeToPOSIXSeconds time)
  get = do
    time <- posixSecondsToUTCTime . fromRational <$> get
    return (Session time)  
