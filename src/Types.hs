module Types where

import Relude
import Data.Time

data Report = Report
  { date :: Day
  , sensors :: Sensors
  } deriving Show

data Sensors = Sensors
  { electricityDay :: Maybe Int
  , electricityNight :: Maybe Int
  , waterCold :: Maybe Float
  , waterHot :: Maybe Float
  , heating :: Maybe Float
  } deriving Show
