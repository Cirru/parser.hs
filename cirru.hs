
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

data CirruBuffer = CirruBuffer { bText :: String
                               , bX    :: Integer
                               , bY    :: Integer
                               }

data CrValue = CrList [CrValue] | CrString String deriving Show
data CirruValue = CirruList [CirruValue] | CirruToken { tText :: String
                                                      , tX    :: Integer
                                                      , tY    :: Integer
                                                      , tEx   :: Integer
                                                      , tEy   :: Integer
                                                      , tPath :: String
                                                      } deriving Show

data CirruState = CirruState { sName     :: String
                             , sX        :: Integer
                             , sY        :: Integer
                             , sLevel    :: Integer
                             , sIndent   :: Integer
                             , sIndented :: Integer
                             , sNest     :: Integer
                             , sPath     :: String
                             }

appendItem :: CirruValue -> Integer -> CirruValue -> CirruValue
appendItem (CirruList xs) 0 x = CirruList (xs ++ [x])
appendItem (CirruList xs) n x =
  CirruList (before ++ after)
  where
    before = init xs
    lastItem = tail xs
    (CirruList after) = appendItem (CirruList lastItem) (n - 1) x

createNesting :: Integer -> CirruValue
createNesting 1 = CirruList []
createNesting n = CirruList [(createNesting (n - 1))]

repeatDollar :: CirruValue -> CirruValue -> CirruValue
repeatDollar (CirruList xs) (CirruList []) = (CirruList xs)
repeatDollar (CirruList before) (CirruList after) =
  case (head after) of
    (CirruList cursor) ->
      repeatDollar (CirruList (before ++ newCursor)) (CirruList (tail after))
      where (CirruList newCursor) = resolveDollar (CirruList cursor)
    (CirruToken "$" _ _ _ _ _) ->
      CirruList (before ++ newAfter)
      where (CirruList newAfter) = resolveDollar (CirruList after)
    (CirruToken s _ _ _ _ _) ->
      repeatDollar (CirruList (before ++ [head after])) (CirruList (tail after))

resolveDollar :: CirruValue -> CirruValue
resolveDollar (CirruList []) = (CirruList [])
resolveDollar (CirruList xs) = repeatDollar (CirruList []) (CirruList xs)

repeatComma :: CirruValue -> CirruValue -> CirruValue
repeatComma (CirruList xs) (CirruList []) = (CirruList xs)
repeatComma (CirruList before) (CirruList after) =
  case (head after) of
    (CirruToken _ _ _ _ _ _) ->
      repeatComma (CirruList (before ++ [head after])) (CirruList (tail after))
    (CirruList cursor) ->
      case (head cursor) of
        (CirruList xs) ->
          repeatComma (CirruList (before ++ newCursor)) (CirruList (tail after))
          where (CirruList newCursor) = resolveComma (CirruList cursor)
        (CirruToken "," _ _ _ _ _) ->
          repeatComma (CirruList before) (CirruList (newCursor ++ (tail after)))
          where (CirruList newCursor) = resolveComma (CirruList (tail cursor))
        (CirruToken _ _ _ _ _ _) ->
          repeatComma (CirruList (before ++ newCursor)) (CirruList (tail after))
          where (CirruList newCursor) = resolveComma (CirruList cursor)

resolveComma :: CirruValue -> CirruValue
resolveComma (CirruList []) = (CirruList [])
resolveComma (CirruList xs) = repeatComma (CirruList []) (CirruList xs)
