
module Cirru
( CirruValue(..)
, CirruState(..)
, CrValue(..)
, CirruBuffer(..)
, createNesting
, appendItem
, resolveDollar
, resolveComma
) where

import Data.Text (pack, unpack)
import Data.Aeson.Types (Array)
import Data.List
import qualified Data.Vector as V
import Debug.Trace
import Data.Aeson
import Data.Traversable (traverse)
import Data.Foldable (toList)
import Control.Applicative

-- Buffer

data CirruBuffer = CirruBuffer { bText :: String
                               , bX    :: Integer
                               , bY    :: Integer
                               }

instance Show CirruBuffer where
  show (CirruBuffer text _ _) = text

-- Short Value

data CrValue = CrList [CrValue] | CrString String

instance Show CrValue where
  show (CrString a) = a
  show (CrList a) = "[" ++ (intercalate "," (map show a)) ++"]"

instance FromJSON CrValue where
  parseJSON v =
        withText "CrString" (pure . CrString . unpack) v
    <|> withArray "CrList" (\a -> CrList . toList <$> traverse parseJSON a) v

instance ToJSON CrValue where
  toJSON (CrString text) = String $ pack text
  toJSON (CrList list) = Array $ V.fromList (map toJSON list)

-- Value

data CirruValue = CirruList [CirruValue] | CirruToken { tText :: String
                                                      , tX    :: Integer
                                                      , tY    :: Integer
                                                      , tEx   :: Integer
                                                      , tEy   :: Integer
                                                      , tPath :: String
                                                      }

instance Show CirruValue where
  show (CirruToken text _ _ _ _ _) = text
  show (CirruList a) = "{" ++ (intercalate "," (map show a)) ++ "}"

instance ToJSON CirruValue where
  toJSON (CirruToken text _ _ _ _ _) = String (pack text)
  toJSON (CirruList list) = Array $ V.fromList (map toJSON list)

-- State

data CirruState = CirruState { sName     :: String
                             , sX        :: Integer
                             , sY        :: Integer
                             , sLevel    :: Integer
                             , sIndent   :: Integer
                             , sIndented :: Integer
                             , sNest     :: Integer
                             , sPath     :: String
                             }

instance Show CirruState where
  show (CirruState name x y level indent indented nest path) =
    name ++ " l:" ++ (show level) ++ " i:" ++ (show indent) ++ "," ++ (show indented) ++ " " ++ (show nest)

-- Manipulations

appendItem :: CirruValue -> Integer -> CirruValue -> CirruValue
appendItem (CirruToken _ _ _ _ _ _) _ _ = error "can not append to token"
appendItem (CirruList xs) 0 x = CirruList (xs ++ [x])
appendItem (CirruList xs) n x =
  CirruList (before ++ after2)
  where
    before = init xs
    lastItem = last xs
    (CirruList after) = appendItem lastItem (n - 1) x
    after2 = [CirruList after]

createNesting :: Integer -> CirruValue
createNesting 1 = CirruList []
createNesting n = CirruList [(createNesting (n - 1))]

repeatDollar :: CirruValue -> CirruValue -> CirruValue
repeatDollar (CirruList xs) (CirruList []) = (CirruList xs)
repeatDollar (CirruList before) (CirruList after) =
  if (length after) == 0
    then CirruList before
    else case (head after) of
      (CirruList cursor) ->
        repeatDollar (CirruList (before ++ [CirruList newCursor])) (CirruList (tail after))
        where (CirruList newCursor) = resolveDollar (CirruList cursor)
      (CirruToken "$" _ _ _ _ _) ->
        CirruList (before ++ [CirruList newAfter])
        where (CirruList newAfter) = resolveDollar (CirruList (tail after))
      (CirruToken s _ _ _ _ _) ->
        repeatDollar (CirruList (before ++ [head after])) (CirruList (tail after))

resolveDollar :: CirruValue -> CirruValue
resolveDollar (CirruList []) = (CirruList [])
resolveDollar (CirruList xs) = repeatDollar (CirruList []) (CirruList xs)

repeatComma :: CirruValue -> CirruValue -> CirruValue
repeatComma (CirruList xs) (CirruList []) = (CirruList xs)
repeatComma (CirruList before) (CirruList after) =
  if (length after) == 0
    then CirruList before
    else case (head after) of
      (CirruToken _ _ _ _ _ _) ->
        repeatComma (CirruList (before ++ [head after])) (CirruList (tail after))
      (CirruList cursor) ->
        if (length cursor) == 0
          then repeatComma (CirruList (before ++ [head after])) (CirruList (tail after))
          else case (head cursor) of
            (CirruList _) ->
              repeatComma (CirruList (before ++ [CirruList newCursor])) (CirruList (tail after))
              where (CirruList newCursor) = resolveComma (CirruList cursor)
            (CirruToken "," _ _ _ _ _) ->
              repeatComma (CirruList before) (CirruList (newCursor ++ (tail after)))
              where (CirruList newCursor) = resolveComma (CirruList (tail cursor))
            (CirruToken _ _ _ _ _ _) ->
              repeatComma (CirruList (before ++ [CirruList newCursor])) (CirruList (tail after))
              where (CirruList newCursor) = resolveComma (CirruList cursor)

resolveComma :: CirruValue -> CirruValue
resolveComma (CirruList []) = (CirruList [])
resolveComma (CirruList xs) = repeatComma (CirruList []) (CirruList xs)
