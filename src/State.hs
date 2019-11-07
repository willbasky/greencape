{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module State
       (
       -- Queries to acid state
         AddReport(..)

       -- DB helpers fo safe quering
       , withDB
       , createCheckpoint'
       , createCheckpointAndClose'
       )
       where

import Control.Exception (bracket)
import Data.Acid as Acid
import Data.Acid.Local as Acid
import Data.SafeCopy
import Data.Time
import Lens.Micro.Platform

import Util

import qualified Data.Map.Strict as Map


data Sensors = Sensors
  { electricityDay   :: Maybe Int
  , electricityNight :: Maybe Int
  , waterCold        :: Maybe Float
  , waterHot         :: Maybe Float
  , heating          :: Maybe Float
  } deriving (Eq, Show, Ord)

$(deriveSafeCopy 0 'base ''Sensors)
makeClassWithLenses ''Sensors

data Reports = Reports
  { reports :: Map Day Sensors
  -- | The dirty bit (needed to choose whether to make a checkpoint or not)
  , dirty   :: Bool
  }
  deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''Reports)
makeClassWithLenses ''Reports

emptyReports :: Reports
emptyReports = Reports {reports = Map.empty, dirty = True}

--------------------------------------------------------------------------------
-- Quary to Acid base in traditional way
--------------------------------------------------------------------------------

addReport :: Day -> Sensors -> Update Reports ()
addReport day sensors = do
  Reports{reports} <- get
  whenNothing_ (Map.lookup day reports) $
    put $ Reports (Map.insert day sensors reports) True

getReport :: Day -> Query Reports (Maybe Sensors)
getReport day = do
  Reports{reports} <- ask
  pure $ Map.lookup day reports

getReports :: Query Reports [Sensors]
getReports = do
  Reports{reports} <- ask
  pure $ Map.elems reports

getDays :: Query Reports [Day]
getDays = do
  Reports{reports} <- ask
  pure $ Map.keys reports

countReports :: Query Reports Int
countReports = do
  Reports{reports} <- ask
  pure $ length reports

changeReportByDay :: Day -> Sensors -> Update Reports ()
changeReportByDay day sensors = do
  Reports{reports} <- get
  whenJust (Map.lookup day reports) $ \_ ->
    put $ Reports (Map.alter (const $ Just sensors) day reports) True

deleteReport :: Day -> Update Reports ()
deleteReport day = do
  Reports{reports} <- get
  put $ Reports (Map.delete day reports) True

-----------------------------------------------------------------------------------
-- Quary to Acid base via lens
-----------------------------------------------------------------------------------

getReportLens :: Day -> Query Reports (Maybe Sensors)
getReportLens day = view (reportByDay day)

reportByDay :: Day -> Lens' Reports (Maybe Sensors)
reportByDay day = singular $
  maybeReportByDay day `failing`
  error ("reportByDay: couldn't find report with day " <> show day)

maybeReportByDay :: Day -> Traversal' Reports (Maybe Sensors)
maybeReportByDay day = _reports . at day

setDirty :: Update Reports ()
setDirty = do
  Reports{..} <- get
  put $ Reports reports True

unsetDirty :: Update Reports Bool
unsetDirty = do
  Reports{..} <- get
  put $ Reports reports False
  pure dirty

-- This will define @getReport@ and @AddReport@ for us.
$(makeAcidic ''Reports
  [ 'addReport
  , 'getReport
  , 'getReportLens
  , 'setDirty
  , 'unsetDirty
  , 'getReports
  , 'getDays
  , 'countReports
  , 'changeReportByDay
  , 'deleteReport
  ])

------------------------------------------------------------------------------
-- Test
------------------------------------------------------------------------------

main :: IO ()
main = do
  day <- utctDay <$> getCurrentTime
  withDB (pure ()) $ \db -> mapM_ (\x -> x >> putTextLn "")
      [ insertReportIO db day sensors1
      , getReportIO db day
      , countReportsIO db

      , insertReportIO db day sensors2
      , getReportIO db day
      , countReportsIO db
      -- , getReportsIO db
      -- , getDaysIO db
      , changeReportIO db (succ day) sensors1
      , getReportIO db (succ day)

      , deleteReportIO db day
      , getReportIO db day
      , countReportsIO db
      ]

getReportIO :: DB -> Day -> IO ()
getReportIO db day = do
  sensors <- query db (GetReportLens day)
  putTextLn $ "Report by day " <> show day <> " is "<> show sensors

getReportsIO :: DB -> IO ()
getReportsIO db = do
  putTextLn "All reports"
  sensors <- query db GetReports
  putTextLn $ show sensors

getDaysIO :: DB -> IO ()
getDaysIO db = do
  days <- query db GetDays
  putTextLn $ "All days: " <> show days

countReportsIO :: DB -> IO ()
countReportsIO db = do
  count <- query db CountReports
  putTextLn $ "Length reports: " <> show count

insertReportIO :: DB -> Day -> Sensors -> IO ()
insertReportIO db day sensors = do
  update db (AddReport day sensors)
  putTextLn $ "Sensors of day " <> show day <> " maybe inserted"

changeReportIO :: DB -> Day -> Sensors -> IO ()
changeReportIO db day sensors = do
  update db (ChangeReportByDay day sensors)
  putTextLn $ "Report of day " <> show day <> " maybe changed"

deleteReportIO :: DB -> Day -> IO ()
deleteReportIO db day = do
  update db (DeleteReport day)
  putTextLn $ "Report of day " <> show day <> " was deleted"

sensors1 :: Sensors
sensors1 = Sensors
  { electricityDay = Just 200
  , electricityNight =  Just 300
  , waterCold = Just 10.0
  , waterHot = Just 5.0
  , heating = Just 80.3
  }

sensors2 :: Sensors
sensors2 = Sensors
  { electricityDay = Just 100
  , electricityNight =  Just 200
  , waterCold = Just 2.0
  , waterHot = Just 1.0
  , heating = Just 180.3
  }

----------------------------------------------------------------------------
-- DB helpers (have to be at the end of the file)
----------------------------------------------------------------------------

-- | A connection to an open acid-state database (allows making
-- queries/updates, creating checkpoints, etc).
type DB = AcidState Reports

-- | Open the database, do something with it, then close the database.
--
-- See Note [acid-state] for the explanation of 'openLocalStateFrom',
-- 'createCheckpoint', etc.
withDB
  :: IO ()               -- ^ Action to run after closing the database
  -> (DB -> IO ())       -- ^ Action to run when the database is open
  -> IO ()
withDB afterClose action = do
  let prepare = openLocalStateFrom "state/" emptyReports
      finalise db = do
        putTextLn "Creating an acid-state checkpoint and closing acid-state"
        createCheckpointAndClose' db
        afterClose
  bracket prepare finalise action

-- | Like 'createCheckpoint', but doesn't create a checkpoint if there were
-- no changes made.
createCheckpoint' :: MonadIO m => DB -> m ()
createCheckpoint' db = liftIO $ do
  wasDirty <- update db UnsetDirty
  when wasDirty $ do
    createArchive db
    createCheckpoint db

-- | Like 'createCheckpointAndClose', but doesn't create a checkpoint if
-- there were no changes made.
createCheckpointAndClose' :: MonadIO m => DB -> m ()
createCheckpointAndClose' db = liftIO $ do
  wasDirty <- update db UnsetDirty
  if wasDirty then do
    createArchive db
    putTextLn "Dirty. Create checkpoint."
    createCheckpointAndClose db
  else closeAcidState db

