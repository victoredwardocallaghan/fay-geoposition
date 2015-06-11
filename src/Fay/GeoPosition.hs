{-|
  Module      : $Header$
  Copyright   : (c) 2015 Altera Praxis Pty Ltd
  License     : BSD
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates Fay Javascript for the
  GeoPosition API specified as in the W3C spec.
-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Fay.GeoPosition ( geoPositionErrorMessage
                       , geoPositionErrorCode
#ifdef FAY
                       , initGeoPosition
#endif
                       , GeoPositionCallbacks(..)
                       , GeoPositionOptions(..)
                       , GeoPosition(..)
                       , GeoCoordinates(..)
                       , GeoPositionError(..)
                       , GeoPositionErrorCode(..)
                       ) where



import Fay.Text      (Text,unpack)
#ifdef FAY
import FFI
import Data.Nullable (toNullable)
#endif
import Data.Time     (UTCTime)
import Data.Data
import Prelude


#ifdef FAY
-- | Call to initialise GeoPosition API with callback
--   actions to be taken upon success or error.
initGeoPosition :: GeoPositionCallbacks Fay
                -> Maybe GeoPositionOptions
                -> Fay ()
initGeoPosition gpc gpo = do
  getCurrentPosition' gpc's gpc'e gpc'o
  where getCurrentPosition' :: GeoPositionCallback Fay
                            -> Nullable (GeoPositionErrorCallback Fay)
                            -> Nullable GeoPositionOptions
                            -> Fay ()
        getCurrentPosition' = ffi "navigator.geolocation.getCurrentPosition(%1,%2,%3)"
        gpc's = geoPositionCallback gpc
        gpc'e = toNullable (geoPositionErrorCallback gpc)
        gpc'o = toNullable gpo

#endif


-- | See section [5.1 - W3C Geolocation API]
-- Requires the monad type that the callbacks have action on
data GeoPositionCallbacks m = GeoPositionCallbacks { geoPositionCallback      :: GeoPositionCallback m
                                                   , geoPositionErrorCallback :: Maybe (GeoPositionErrorCallback m)
                                                   }

type GeoPositionCallback      m = GeoPosition      -> m ()
type GeoPositionErrorCallback m = GeoPositionError -> m ()

-- | See section [5.2 - W3C Geolocation API]
data GeoPositionOptions =
  GeoPositionOptions { -- | Is a 'Bool' that indicates the application would
		       -- like to receive the best possible results.
                       enableHighAccuracy :: Bool
		       -- | Maximum length of time (in milliseconds) the device
		       -- is allowed to take in order to return a position.
                       , timeout            :: Int
		       -- | Maximum age in milliseconds of a possible cached
		       -- position that is acceptable to return.
                       , maximumAge         :: Int
                       } deriving Eq

-- | See section [5.3 - W3C Geolocation API]
data GeoPosition = GeoPosition { coords    :: GeoCoordinates -- ^ Coordinates object defining the current location
                               , timestamp :: UTCTime        -- ^ DOMTimeStamp, time at which location was retrieved
                               } deriving (Show,Data,Typeable)

-- | See section [5.4 - W3C Geolocation API]
data GeoCoordinates = GeoCoordinates { latitude         :: Double       -- ^ decimal degrees
                                     , longitude        :: Double       -- ^ decimal degrees
                                     , altitude         :: Maybe Double -- ^ meters above the reference ellipsoid
                                     , accuracy         :: Double       -- ^ meters
                                     , altitudeAccuracy :: Maybe Double -- ^ meters
                                     , heading          :: Maybe Double -- ^ degrees clockwise from true north
                                     , speed            :: Maybe Double -- ^ meters/second
                                     } deriving (Read,Show,Data,Typeable)

-- | See section [5.5 - W3C Geolocation API]
data GeoPositionError = GeoPositionError { code    :: Int  -- ^ Enumerated code, see 'GeoPositionErrorCode'
                                         , message :: Text -- ^ Human readable 'UTF-16 DOMString' for debugging
                                         } deriving (Read,Show,Data,Typeable)

-- | N.B. Fay does not support the Enum type class :(
-- Thus, use the 'geoPositionErrorCode' function to decode the value to the
-- 'GeoPositionErrorCode' type.
data GeoPositionErrorCode
  -- | The acquisition of the geolocation information failed because the page
  -- didn't have the permission to do it.
  = PermissionDenied
  -- | The acquisition of the geolocation failed because at least one internal
  -- source of position returned an internal error.
  | PositionUnavailable
  -- | The time allowed to acquire the geolocation, defined by 'timeout'
  -- information was reached before the information was obtained.
  | Timeout
  -- | Out of spec error code
  | Unknown
   deriving (Eq)

-- | Helper function extracts UTC-16 encoded error message
geoPositionErrorMessage :: GeoPositionError -> String
geoPositionErrorMessage = unpack . message

-- | Helper function decodes enumerated error 'code' into the 'GeoPositionErrorCode' type
geoPositionErrorCode :: GeoPositionError -> GeoPositionErrorCode
geoPositionErrorCode = decode . code
  where decode e | e == 1    = PermissionDenied
                 | e == 2    = PositionUnavailable
                 | e == 3    = Timeout
                 | otherwise = Unknown
