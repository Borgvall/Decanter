{-# LANGUAGE OverloadedStrings #-}

module Data.Vdf (extractDisplayName) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Extrahiert den "display_name" aus einem VDF-formatierten Text.
-- Gibt den gefundenen Namen oder einen leeren Text zurück.
--
-- Beispiel Input:
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
        -- Kommentare entfernen (alles nach //)
        let (code, _) = T.breakOn "//" line
            -- An Anführungszeichen splitten
            -- Beispiel: "  \"display_name\" \"GE-Proton10-25\" "
            -- Split ergibt: ["  ", "display_name", " ", "GE-Proton10-25", " "]
            parts = T.splitOn "\"" code
        in case findValue parts of
             Just val -> val
             Nothing  -> go rest

    -- Sucht in den gesplitteten Teilen nach dem Key "display_name"
    findValue :: [Text] -> Maybe Text
    findValue [] = Nothing
    findValue (p:ps)
        | p == "display_name" = getNextString ps
        | otherwise = findValue ps

    -- Sucht den nächsten nicht-leeren String (den Wert)
    getNextString :: [Text] -> Maybe Text
    getNextString [] = Nothing
    getNextString (p:ps)
        | T.null (T.strip p) = getNextString ps -- Überspringe Whitespace zwischen Key und Value
        | otherwise = Just p -- Der nächste Inhalt ist der Value (ohne Quotes)
