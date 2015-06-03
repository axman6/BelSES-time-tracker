{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies #-}

module App.User where


import App.Common

import           Data.IntMap as IM


data User = User
	{ _firstName :: Text
 	, _lastName	 :: Text
	} deriving (Eq, Ord, Typeable, Show)

makeLenses ''User
deriveSafeCopy 0 'base ''User

newtype Users = Users {unUsers :: IntMap User}
	deriving (Eq, Typeable)
deriveSafeCopy 0 'base ''Users

setFirstName :: Text -> Update User ()
setFirstName f = modify (firstName .~ f)

setLastName :: Text -> Update User ()
setLastName f = modify (lastName .~ f)

getFirst :: Query User Text
getFirst = asks _firstName

getLast :: Query User Text
getLast = asks _firstName

makeAcidic ''User ['setFirstName, 'setLastName]


addNext :: User -> Update Users Int
addNext u = do
	Users im <- get
	case maxViewWithKey im of
		Nothing 	   -> put (Users $ IM.singleton 0 u) >> return 0
		Just ((k,_),_) -> put (Users $ IM.insert (k+1) u im) >> return (k+1)

getValue :: Int -> Query Users (Maybe User)
getValue k = asks (IM.lookup k . unUsers)


makeAcidic ''Users ['addNext, 'getValue]

