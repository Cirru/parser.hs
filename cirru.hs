
module Cirru
( CirruValue(..)
, appendItem
, resolveDollar
, resolveComma
) where

data CirruValue = CirruList [CirruValue] | CirruString String deriving Show

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
    (CirruString "$") ->
      CirruList (before ++ newAfter)
      where (CirruList newAfter) = resolveDollar (CirruList after)
    (CirruString s) ->
      repeatDollar (CirruList (before ++ [newS])) (CirruList (tail after))
      where newS = CirruString s

resolveDollar :: CirruValue -> CirruValue
resolveDollar (CirruList []) = (CirruList [])
resolveDollar (CirruList xs) = repeatDollar (CirruList []) (CirruList xs)

repeatComma :: CirruValue -> CirruValue -> CirruValue
repeatComma (CirruList xs) (CirruList []) = (CirruList xs)
repeatComma (CirruList before) (CirruList after) =
  case (head after) of
    (CirruString s) ->
      repeatComma (CirruList (before ++ [newS])) (CirruList (tail after))
      where newS = CirruString s
    (CirruList cursor) ->
      case (head cursor) of
        (CirruList xs) ->
          repeatComma (CirruList (before ++ newCursor)) (CirruList (tail after))
          where (CirruList newCursor) = resolveComma (CirruList cursor)
        (CirruString ",") ->
          repeatComma (CirruList before) (CirruList (newCursor ++ (tail after)))
          where (CirruList newCursor) = resolveComma (CirruList (tail cursor))
        (CirruString s) ->
          repeatComma (CirruList (before ++ newCursor)) (CirruList (tail after))
          where (CirruList newCursor) = resolveComma (CirruList cursor)

resolveComma :: CirruValue -> CirruValue
resolveComma (CirruList []) = (CirruList [])
resolveComma (CirruList xs) = repeatComma (CirruList []) (CirruList xs)
