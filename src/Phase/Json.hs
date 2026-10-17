{-# LANGUAGE OverloadedStrings #-}

-- | JSON integration for PhaseDB.
--
-- Supports converting between PhaseDB hierarchical 'Node' trees and
-- 'Data.Aeson.Value' JSON structures, and Parsec parsing of embedded
-- JSON in Phase statements ('ZMERGE').
module Phase.Json
  ( -- * Tree / JSON Conversions
    jsonToNode
  , nodeToJson
  , renderNodeJson
  , parseJsonText

    -- * Parsec Parsers
  , pJsonValue
  , pJsonObject
  , pJsonArray
  , pJsonString
  , pJsonNumber
  , pJsonBool
  , pJsonNull
  ) where

import Control.Applicative ((<|>))
import Data.Char (chr)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, base10Exponent, coefficient, normalize)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Numeric (readHex)
import Text.Parsec
  ( digit
  , hexDigit
  , many
  , many1
  , noneOf
  , option
  , satisfy
  , sepBy
  , skipMany
  , string
  , try
  , unexpected
  )
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

import Phase.AST (PValue (..), mkValue)
import Phase.Parser (PParser)
import Phase.Tree (Node (..), SubscriptKey (..), toSubKey)
import Phase.Value (CanonicalNumber (..), canonicalNumber)

-- --------------------------------------------------------------------
-- Tree / JSON Conversion
-- --------------------------------------------------------------------

-- | Format a 'Scientific' as canonical numeric text (no exponent notation,
-- trimmed trailing fractional zeroes, no spurious ".0" for integers).
-- Mirrors the same logic as 'Phase.Eval.formatSci'.
formatScientificText :: Scientific -> Text
formatScientificText s =
  let norm = normalize s
      c    = coefficient norm
      e    = base10Exponent norm
      isNeg = c < 0
      absC  = abs c
      sign  = if isNeg then "-" else ""
  in if e >= 0
       then T.pack (sign ++ show absC ++ replicate e '0')
       else
         let digits  = show absC
             numDigs = length digits
             dotPos  = numDigs + e
         in if dotPos <= 0
              then T.pack (sign ++ "0." ++ replicate (negate dotPos) '0' ++ digits)
              else
                let (intP, fracP) = splitAt dotPos digits
                in T.pack (sign ++ intP ++ (if null fracP then "" else "." ++ fracP))

-- | Convert an 'Aeson.Value' into a hierarchical PhaseDB 'Node'.
--
-- - JSON Object: creates child nodes keyed by object keys.
-- - JSON Array: creates child nodes with 1-based sequential keys ("1", "2", ...).
-- - JSON String: leaf node with string value.
-- - JSON Number: leaf node with canonical numeric value.
-- - JSON Bool: leaf node with "1" (true) or "0" (false).
-- - JSON Null: leaf node with empty string "" (per PhaseDB ZMERGE spec).
jsonToNode :: Aeson.Value -> Node
jsonToNode (Aeson.Object km) =
  let children = Map.fromList
        [ (toSubKey (mkValue (Key.toText k)), jsonToNode v)
        | (k, v) <- KM.toList km
        ]
  in Node Nothing children

jsonToNode (Aeson.Array vec) =
  let children = Map.fromList
        [ (toSubKey (mkValue (T.pack (show i))), jsonToNode v)
        | (i, v) <- zip [1 :: Int ..] (Foldable.toList vec)
        ]
  in Node Nothing children

jsonToNode (Aeson.String txt) =
  Node (Just (mkValue txt)) Map.empty

jsonToNode (Aeson.Number sci) =
  Node (Just (mkValue (formatScientificText sci))) Map.empty

jsonToNode (Aeson.Bool True) =
  Node (Just (mkValue "1")) Map.empty

jsonToNode (Aeson.Bool False) =
  Node (Just (mkValue "0")) Map.empty

jsonToNode Aeson.Null =
  Node (Just (mkValue "")) Map.empty

-- | Convert a PhaseDB 'Node' tree into a JSON 'Aeson.Value'.
--
-- Internal nodes with children become JSON Objects (subscripts as string keys).
-- Leaf nodes become Numbers if their value is in Phase's canonical numeric form,
-- or Strings otherwise. If a node has both children and an own value (node duality),
-- its own value is included under the "_value" field.
nodeToJson :: Node -> Aeson.Value
nodeToJson node
  | not (Map.null (nodeChildren node)) =
      let childPairs =
            [ (Key.fromText (unPValue (unSubscriptKey k)), nodeToJson child)
            | (k, child) <- Map.toList (nodeChildren node)
            ]
          allPairs = case nodeValue node of
            Just v  -> (Key.fromText "_value", leafToJson v) : childPairs
            Nothing -> childPairs
      in Aeson.Object (KM.fromList allPairs)
  | otherwise =
      case nodeValue node of
        Just v  -> leafToJson v
        Nothing -> Aeson.object []
  where
    leafToJson val =
      case canonicalNumber val of
        Just (CanonicalNumber sci) -> Aeson.Number (normalize sci)
        Nothing                    -> Aeson.String (unPValue val)

-- | Render a PhaseDB 'Node' tree to compact UTF-8 JSON text.
renderNodeJson :: Node -> Text
renderNodeJson node =
  TE.decodeUtf8 (LBS.toStrict (Aeson.encode (nodeToJson node)))

-- | Parse a JSON string into an 'Aeson.Value'.
parseJsonText :: Text -> Either String Aeson.Value
parseJsonText txt =
  Aeson.eitherDecode (LBS.fromStrict (TE.encodeUtf8 txt))

-- --------------------------------------------------------------------
-- Parsec JSON Parsers
-- --------------------------------------------------------------------

pJsonSpace :: PParser ()
pJsonSpace = skipMany (satisfy isSpaceChar)
  where
    isSpaceChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

pJsonValue :: PParser Aeson.Value
pJsonValue =
      pJsonObject
  <|> pJsonArray
  <|> (Aeson.String <$> pJsonString)
  <|> pJsonBool
  <|> pJsonNull
  <|> pJsonNumber

pJsonObject :: PParser Aeson.Value
pJsonObject = do
  _ <- string "{"
  pJsonSpace
  pairs <- pPair `sepBy` try (pJsonSpace *> string "," <* pJsonSpace)
  pJsonSpace
  _ <- string "}"
  pure (Aeson.Object (KM.fromList pairs))
  where
    pPair = do
      k <- pJsonString
      pJsonSpace
      _ <- string ":"
      pJsonSpace
      v <- pJsonValue
      pure (Key.fromText k, v)

pJsonArray :: PParser Aeson.Value
pJsonArray = do
  _ <- string "["
  pJsonSpace
  elements <- pJsonValue `sepBy` try (pJsonSpace *> string "," <* pJsonSpace)
  pJsonSpace
  _ <- string "]"
  pure (Aeson.toJSON elements)

pJsonString :: PParser Text
pJsonString = do
  _ <- string "\""
  chars <- many (pEscape <|> noneOf "\"\\")
  _ <- string "\""
  pure (T.pack chars)
  where
    pEscape = do
      _ <- string "\\"
      c <- satisfy (`elem` ("\"\\/bfnrtu" :: String))
      case c of
        '"'  -> pure '"'
        '\\' -> pure '\\'
        '/'  -> pure '/'
        'b'  -> pure '\b'
        'f'  -> pure '\f'
        'n'  -> pure '\n'
        'r'  -> pure '\r'
        't'  -> pure '\t'
        'u'  -> pUnicode
        _    -> unexpected [c]

    pUnicode = do
      d1 <- hexDigit
      d2 <- hexDigit
      d3 <- hexDigit
      d4 <- hexDigit
      case readHex [d1, d2, d3, d4] of
        [(n, "")] -> pure (chr n)
        _         -> unexpected "hex escape in JSON string"

pJsonNumber :: PParser Aeson.Value
pJsonNumber = do
  sign <- option "" (string "-")
  intPart <- string "0" <|> many1 digit
  fracPart <- option "" $ do
    dot <- string "."
    digs <- many1 digit
    pure (dot ++ digs)
  expPart <- option "" $ do
    e <- string "e" <|> string "E"
    esign <- option "" (string "+" <|> string "-")
    edigs <- many1 digit
    pure (e ++ esign ++ edigs)
  let rawStr = sign ++ intPart ++ fracPart ++ expPart
  case readMaybe rawStr :: Maybe Scientific of
    Just sci -> pure (Aeson.Number sci)
    Nothing  -> unexpected "invalid JSON number"

pJsonBool :: PParser Aeson.Value
pJsonBool =
      (Aeson.Bool True  <$ string "true")
  <|> (Aeson.Bool False <$ string "false")

pJsonNull :: PParser Aeson.Value
pJsonNull = Aeson.Null <$ string "null"
