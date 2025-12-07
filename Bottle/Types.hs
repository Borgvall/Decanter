{-# LANGUAGE OverloadedStrings #-}

module Bottle.Types where

import Data.Text (Text)

-- Dummy-Funktion für i18n
tr :: Text -> Text
tr = id 

-- Architekturen
data Arch = Win32 | Win64
  deriving (Show, Eq, Enum)

archToString :: Arch -> String
archToString arch = case arch of
  Win32 -> "win32"
  Win64 -> "win64"

-- Runner
data RunnerType = SystemWine | Proton | GeProton
  deriving (Show, Eq, Enum)

-- Bottle Definition
data Bottle = Bottle
  { bottleName :: Text
  , bottlePath :: FilePath
  , runner     :: RunnerType
  , arch       :: Arch
  } deriving (Show, Eq)
