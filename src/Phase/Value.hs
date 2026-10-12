{-# LANGUAGE OverloadedStrings #-}

-- | Value coercion rules for Phase. This is the single funnel
-- 'Phase.AST.PValue' must pass through for every "treat this string
-- as a number" or "treat this string as a subscript" decision.
--
-- Everything in this module is transcribed directly from handbook
-- Part I §2 and §3.2, including the deliberately-kept M quirks. Do
-- not "fix" any of the below, the handbook is explicit that these
-- are documented, intentional behaviors, not parsing shortcuts:
--
--   "12abc"  → coerces to 12       (leading numeric prefix, rest ignored)
--   "abc"    → coerces to 0        (no valid leading numeric prefix)
--   "  12"   → coerces to 0        (!) leading whitespace is NOT skipped
--
-- and canonical numeric form for subscript collation (§3.2):
-- no leading zero, no leading '+', no trailing zero after a decimal
-- point, no trailing '.', no exponent notation.
module Phase.Value
  ( -- * Numeric coercion (§2)
    toNumber
  , isTruthy

    -- * Canonical numeric form (§3.2)
  , CanonicalNumber
  , canonicalNumber
  , canonicalText

    -- * Equality and collation (§2, §3.2)
  , phaseEq
  , collationCompare
  ) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Phase.AST (PValue (..))

-- --------------------------------------------------------------------
-- Numeric coercion (§2)
-- --------------------------------------------------------------------

-- | Coerce a 'PValue' to a number by reading its LEADING numeric
-- prefix and ignoring everything after it. Returns 0 when no valid
-- leading numeric prefix exists, including the case of leading
-- whitespace, which is NOT skipped (that's the "(!)" quirk the
-- handbook calls out explicitly).
--
-- We use 'Scientific' rather than 'Double' to line up with the
-- handbook's note that Phase arithmetic runs on @BigDecimal@ with 34
-- digits of precision, 'Scientific' is Haskell's closest arbitrary-
-- precision decimal type and is what 'Phase.Eval' should build
-- arithmetic on top of. This function only handles the *coercion*
-- half; rounding mode (HALF_UP, 34 digits) is an arithmetic-operator
-- concern, not a coercion concern, and belongs in the evaluator.
toNumber :: PValue -> Scientific
toNumber (PValue t) = case leadingNumericPrefix t of
  Nothing -> 0
  Just s  ->
    let s' = case T.uncons s of
               Just ('+', rest) | T.isPrefixOf "." rest -> "+0" <> rest
               Just ('-', rest) | T.isPrefixOf "." rest -> "-0" <> rest
               _ | T.isPrefixOf "." s                   -> "0" <> s
               _                                        -> s
    in case reads (T.unpack s') of
      ((n, _) : _) -> n
      []           -> 0

-- | Extract the longest prefix of the input that parses as a valid
-- number: optional sign, digits, optional decimal point and more
-- digits. Deliberately does NOT skip leading whitespace, per the
-- handbook, "  12" coerces to 0, not 12.
--
-- Grammar (regex-equivalent): @^[+-]?[0-9]*\.?[0-9]+@ or
-- @^[+-]?[0-9]+\.?[0-9]*@, i.e. we need at least one digit somewhere,
-- either before or after an optional decimal point.
leadingNumericPrefix :: Text -> Maybe Text
leadingNumericPrefix t0 =
  let (sign, t1) = case T.uncons t0 of
        Just (c, rest) | c == '+' || c == '-' -> (T.singleton c, rest)
        _ -> (T.empty, t0)
      (intPart, t2) = T.span isDigit t1
      (fracPart, hasDot) = case T.uncons t2 of
        Just ('.', rest) ->
          let (digs, _) = T.span isDigit rest
          in (digs, True)
        _ -> (T.empty, False)
      numDigits = T.length intPart + T.length fracPart
  in if numDigits == 0
       then Nothing
       else Just $ sign
                <> intPart
                <> (if hasDot then "." <> fracPart else T.empty)

-- | Truthiness (§2): coerce to a number, then "nonzero is true."
isTruthy :: PValue -> Bool
isTruthy v = toNumber v /= 0

-- --------------------------------------------------------------------
-- Canonical numeric form (§3.2)
-- --------------------------------------------------------------------

-- | A value that IS in Phase's canonical numeric form. This is a
-- distinct, narrower notion than "coerces to a nonzero number" (§2):
-- canonical form is a syntactic property of the literal TEXT, checked
-- for exact shape, not a coercion of arbitrary garbage. "01", "+1",
-- "1.50", "1.0", and "1E2" all coerce fine under 'toNumber' but are
-- explicitly NOT canonical, and the handbook is emphatic that this
-- distinction is what decides both subscript collation (this section)
-- and top-level '=' equality (§2), so we keep it as its own type
-- rather than reusing 'Scientific', to make it impossible to
-- accidentally compare a canonical-checked value against a merely-
-- coerced one.
newtype CanonicalNumber = CanonicalNumber Scientific
  deriving (Eq, Ord, Show)

-- | Recognize whether a value's text is in canonical numeric form,
-- per the handbook's exact rule: "no leading zero, no leading '+', no
-- trailing zero after a decimal point, no trailing '.', no exponent
-- notation." A canonical number may still be negative (the handbook's
-- own examples include "-4"), so a leading '-' is fine; it's a
-- leading '+' that disqualifies.
canonicalNumber :: PValue -> Maybe CanonicalNumber
canonicalNumber (PValue t)
  | T.null t = Nothing
  | otherwise =
      let (neg, body) = case T.uncons t of
            Just ('-', rest) -> (True, rest)
            _                -> (False, t)
      in if T.any (`elem` ("eE" :: String)) body
           then Nothing -- exponent notation disqualifies
           else case T.splitOn "." body of
             [intPart] ->
               validCanonicalInt intPart >> mkCanonical neg intPart "0"
             [intPart, fracPart] ->
               if T.null fracPart
                 then Nothing -- trailing "." disqualifies
                 else if T.last fracPart == '0'
                        then Nothing -- trailing zero after decimal point disqualifies
                        else validCanonicalInt intPart
                               >> validFrac fracPart
                               >> mkCanonical neg intPart fracPart
             _ -> Nothing -- more than one '.'
  where
    -- Integer part rules: non-empty, all digits, and no leading zero
    -- UNLESS the whole integer part is exactly "0".
    validCanonicalInt :: Text -> Maybe ()
    validCanonicalInt intPart
      | T.null intPart = Nothing
      | not (T.all isDigit intPart) = Nothing
      | intPart == "0" = Just ()
      | T.head intPart == '0' = Nothing
      | otherwise = Just ()

    validFrac :: Text -> Maybe ()
    validFrac fracPart
      | not (T.all isDigit fracPart) = Nothing
      | otherwise = Just ()

    mkCanonical :: Bool -> Text -> Text -> Maybe CanonicalNumber
    mkCanonical neg intPart fracPart = do
      let signTxt = if neg then "-" else ""
          fullTxt = signTxt <> intPart <> "." <> fracPart
      case reads (T.unpack fullTxt) of
        ((n, "") : _) -> Just (CanonicalNumber n)
        _             -> Nothing

canonicalText :: CanonicalNumber -> Text
canonicalText (CanonicalNumber n) = T.pack (show (Sci.normalize n))

-- --------------------------------------------------------------------
-- Equality (§2) and collation (§3.2)
-- --------------------------------------------------------------------

-- | Phase '=' semantics, transcribed exactly from the handbook:
--
-- "Phase checks both sides against the same canonical-numeric-form
-- rule that subscripts use. When both operands already have that
-- canonical numeric form, '=' compares them as numbers. Otherwise, '='
-- falls back to exact string comparison."
--
-- This deliberately reproduces the quirks:
--
-- >>> phaseEq (PValue "1") (PValue "1")      == True   -- both canonical
-- >>> phaseEq (PValue "1") (PValue "1.0")    == False  -- "1.0" not canonical -> string compare, "1" /= "1.0"
-- >>> phaseEq (PValue "01") (PValue "1")     == False  -- "01" not canonical -> string compare, "01" /= "1"
phaseEq :: PValue -> PValue -> Bool
phaseEq a@(PValue ta) b@(PValue tb) =
  case (canonicalNumber a, canonicalNumber b) of
    (Just na, Just nb) -> na == nb
    _                  -> ta == tb

-- | Collation ordering per §3.2, used by both @]]@ (sorts-after) and
-- subscript traversal order (@$ORDER@, Part I §6): every canonical-
-- numeric subscript sorts before every string subscript; within each
-- group, numbers sort by numeric value and strings sort by character
-- order.
collationCompare :: PValue -> PValue -> Ordering
collationCompare a b =
  case (canonicalNumber a, canonicalNumber b) of
    (Just na, Just nb) -> compare na nb
    (Just _, Nothing)  -> LT  -- numbers sort before strings, always
    (Nothing, Just _)  -> GT
    (Nothing, Nothing) -> compare (unPValue a) (unPValue b)
