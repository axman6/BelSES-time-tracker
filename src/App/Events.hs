{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies #-}

module App.Events where

import App.Common

import App.User

-- import           Control.Lens

-- import           Data.Text (Text)
-- import qualified Data.Text as T


-- import           Data.Typeable

-- import           Data.Acid
-- import           Data.SafeCopy -- (base, deriveSafeCopy)

-- import           Control.Monad.Reader (asks, ask)
-- import           Control.Monad.State (modify, get, put)

import Data.List (intercalate)

import Data.Time.Clock


data EventSort =
	SignIn Text
	| SignOut
	| Other Text
	deriving (Show, Typeable, Eq)

makeLenses ''EventSort
deriveSafeCopy 0 'base ''EventSort

data Event_v0 =
	Event_v0
		UTCTime
		EventSort
		Int -- User id
	deriving (Show, Typeable, Eq)

makeLenses ''Event_v0
deriveSafeCopy 0 'base ''Event_v0
-- makeAcidic ''Event_v0 []


data Event =
	Event
		UTCTime
		Int -- User id
		EventSort
	deriving (Show, Typeable, Eq)

instance Migrate Event where
	type MigrateFrom Event = Event_v0
	migrate (Event_v0 t s i) = Event t i s

makeLenses ''Event
deriveSafeCopy 1 'extension ''Event

newtype Events_v0 = Events_v0 {_events_v0 :: [Event_v0]}
	deriving (Typeable)

makeLenses ''Events_v0
deriveSafeCopy 0 'base ''Events_v0
-- makeAcidic ''Events_v0 []

newtype Events = Events {_events :: [Event]}
	deriving (Typeable)

instance Migrate Events where
	type MigrateFrom Events = Events_v0
	migrate (Events_v0 ls) = Events (map migrate ls)

instance Show Events where
	show (Events es) = intercalate "\n" . map show $ es

makeLenses ''Events
deriveSafeCopy 1 'extension ''Events

addEvent :: Event -> Update Events ()
addEvent e = modify (events %~ (e:))

getEvents :: Query Events Events
getEvents = ask

makeAcidic ''Events ['addEvent, 'getEvents]


addEventNow :: AcidState Events -> Int -> EventSort -> IO ()
addEventNow evts who evt = do
	now <- getCurrentTime
	update evts (AddEvent (Event now who evt))

