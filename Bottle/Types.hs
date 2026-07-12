{-# LANGUAGE OverloadedStrings #-}

module Bottle.Types where

import Data.Text (Text)

data RunnerType = SystemWine | Proton FilePath
  deriving (Show, Eq, Read)

data Bottle = Bottle
  { bottleName :: Text
  , bottlePath :: FilePath
  , runner     :: RunnerType
  } deriving (Show, Eq)

data BottleSnapshot = BottleSnapshot
  { snapshotId   :: Int
  , snapshotName :: Text
  , snapshotPath :: FilePath
  } deriving (Show, Eq)
