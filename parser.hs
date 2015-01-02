
module Parser
( parse
, pare
) where

import Cirru

isEmpty :: String -> Bool
isEmpty [] = True
isEmpty _ = False

emptyList = CirruList []

parse :: String -> String -> CirruValue
parse code filename =
  parseState emptyList buffer startState code
  where
    buffer = CirruBuffer "" 1 1
    startState = CirruState "indent" 0 0 1 0 0 0 filename


cr :: CirruValue -> CrValue
cr (CirruToken x _ _ _ _ _) = CrString x
cr (CirruList xs) = CrList (map cr xs)

pare :: String -> String -> CrValue
pare code filename = cr (parse code filename)


parseState :: CirruValue -> CirruBuffer -> CirruState -> String -> CirruValue
parseState xs buffer state code =
  if (isEmpty code)
    then case (sName state) of
      ("escape") -> escapeEof xs buffer state code
      ("string") -> stringEof xs buffer state code
      ("space")  -> spaceEof  xs buffer state code
      ("token")  -> tokenEof  xs buffer state code
      ("indent") -> indentEof xs buffer state code
    else case (sName state) of
      ("escape") ->
        case (head code) of
          ('\n') -> escapeNewline xs buffer state code
          ('n')  -> escapeN       xs buffer state code
          ('t')  -> escapeT       xs buffer state code
          _      -> escapeElse    xs buffer state code
      ("string") ->
        case (head code) of
          ('\\') -> stringBackslash xs buffer state code
          ('\n') -> stringNewline   xs buffer state code
          ('\"') -> stringQuote     xs buffer state code
          _      -> stringElse      xs buffer state code
      ("space") ->
        case (head code) of
          (' ')  -> spaceSpace   xs buffer state code
          ('\n') -> spaceNewline xs buffer state code
          ('(')  -> stateOpen    xs buffer state code
          (')')  -> stateClose   xs buffer state code
          ('"')  -> stateQuote   xs buffer state code
          _      -> stateElse    xs buffer state code
      ("token") ->
        case (head code) of
          (' ') ->  tokenSpace   xs buffer state code
          ('\n') -> tokenNewline xs buffer state code
          ('(') ->  tokenOpen    xs buffer state code
          (')') ->  tokenClose   xs buffer state code
          ('"') ->  tokenQuote   xs buffer state code
          _ ->      tokenElse    xs buffer state code
      ("indent") ->
        case (head code) of
          (' ') ->  indentSpace   xs buffer state code
          ('\n') -> indentNewline xs buffer state code
          (')') ->  indentClose   xs buffer state code
          _ ->      indentElse    xs buffer state code

escapeEof xs buffer state code = error "EOF in escape state"

stringEof xs buffer state code = error "EOF in string state"

spaceEof xs buffer state code = xs

tokenEof xs b s code =
  let newToken = CirruToken (bText b) (bX b) (bY b) (sX s) (sY s) (sPath s)
      newXs = appendItem xs (sLevel s) newToken
  in xs

indentEof xs buffer state code = xs


escapeNewline xs buffer state code = error "new line while escape"

escapeN xs b s code =
  let newState = CirruState "string" ((sX s) + 1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer ((bText b) ++ "\n") (bX b) (bY b)
  in parseState xs newBuffer newState (tail code)

escapeT xs buffer state code = emptyList

escapeElse xs buffer state code = emptyList


stringBackslash xs buffer state code = emptyList

stringNewline xs buffer state code = emptyList

stringQuote xs buffer state code = emptyList

stringElse xs buffer state code = emptyList


spaceSpace xs buffer state code = emptyList

spaceNewline xs buffer state code = emptyList

stateOpen xs buffer state code = emptyList

stateClose xs buffer state code = emptyList

stateQuote xs buffer state code = emptyList

stateElse xs buffer state code = emptyList


tokenSpace xs buffer state code = emptyList

tokenNewline xs buffer state code = emptyList

tokenOpen xs buffer state code = emptyList

tokenClose xs buffer state code = emptyList

tokenQuote xs buffer state code = emptyList

tokenElse xs buffer state code = emptyList


indentSpace xs buffer state code = emptyList

indentNewline xs buffer state code = emptyList

indentClose xs buffer state code = emptyList

indentElse xs buffer state code = emptyList
