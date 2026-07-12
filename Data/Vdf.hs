{-# LANGUAGE OverloadedStrings #-}

module Data.Vdf (extractDisplayName) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Extracts the "display_name" from a VDF-formatted text.
-- Returns the found name, or an empty text if none is found.
--
-- Example input:
-- "compatibilitytools"
-- {
--   ...
--   "display_name" "GE-Proton10-25"
--   ...
-- }
--
-- Output: "GE-Proton10-25"
extractDisplayName :: Text -> Text
extractDisplayName content = go (T.lines content)
  where
    go [] = ""
    go (line:rest) =
        -- Strip comments (everything after //)
        let (code, _) = T.breakOn "//" line
            -- Split on quotes, e.g. "  \"display_name\" \"GE-Proton10-25\" "
            -- splits into ["  ", "display_name", " ", "GE-Proton10-25", " "]
            parts = T.splitOn "\"" code
        in case findValue parts of
             Just val -> val
             Nothing  -> go rest

    -- Looks for the "display_name" key among the split parts
    findValue :: [Text] -> Maybe Text
    findValue [] = Nothing
    findValue (p:ps)
        | p == "display_name" = getNextString ps
        | otherwise = findValue ps

    -- Looks for the next non-empty string (the value)
    getNextString :: [Text] -> Maybe Text
    getNextString [] = Nothing
    getNextString (p:ps)
        | T.null (T.strip p) = getNextString ps -- skip whitespace between key and value
        | otherwise = Just p -- the next content is the value (without quotes)
