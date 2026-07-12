{-# LANGUAGE OverloadedStrings #-}

module Bottle.Types where

import Data.Text (Text)

data Arch = Win32 | Win64
  deriving (Show, Eq, Enum, Read)

archToString :: Arch -> String
archToString a = case a of
  Win32 -> "win32"
  Win64 -> "win64"

data RunnerType = SystemWine | Proton FilePath
  deriving (Show, Eq, Read)

data Bottle = Bottle
  { bottleName :: Text
  , bottlePath :: FilePath
  , runner     :: RunnerType
  , arch       :: Arch
  } deriving (Show, Eq)

data BottleSnapshot = BottleSnapshot
  { snapshotId   :: Int
  , snapshotName :: Text
  , snapshotPath :: FilePath
  } deriving (Show, Eq)
