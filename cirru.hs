
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

import Data.List
import Debug.Trace

data CirruBuffer = CirruBuffer { bText :: String
                               , bX    :: Integer
                               , bY    :: Integer
                               }

instance Show CirruBuffer where
  show (CirruBuffer text _ _) = text

data CrValue = CrList [CrValue] | CrString String

instance Show CrValue where
  show (CrString a) = a
  show (CrList a) = "[" ++ (intercalate "," (map show a)) ++"]"

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
  trace ((show before) ++ " - " ++ (show after)) $ if (length after) == 0
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
              repeatComma (CirruList before) (CirruList ([CirruList newCursor] ++ (tail after)))
              where (CirruList newCursor) = resolveComma (CirruList (tail cursor))
            (CirruToken _ _ _ _ _ _) ->
              repeatComma (CirruList (before ++ [CirruList newCursor])) (CirruList (tail after))
              where (CirruList newCursor) = resolveComma (CirruList cursor)

resolveComma :: CirruValue -> CirruValue
resolveComma (CirruList []) = (CirruList [])
resolveComma (CirruList xs) = repeatComma (CirruList []) (CirruList xs)
