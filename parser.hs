
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
    startState = CirruState "indent" 1 1 1 0 0 0 filename


cr :: CirruValue -> CrValue
cr (CirruToken x _ _ _ _ _) = CrString x
cr (CirruList xs) = CrList (map cr xs)

pare :: String -> String -> CrValue
pare code filename = cr (parse code filename)

newState x = x

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
          ('(')  -> spaceOpen    xs buffer state code
          (')')  -> spaceClose   xs buffer state code
          ('"')  -> spaceQuote   xs buffer state code
          _      -> spaceElse    xs buffer state code
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
      _ -> error ("unknown state: " ++ (sName state))

escapeEof xs buffer state code = error "EOF in escape state"

stringEof xs buffer state code = error "EOF in string state"

spaceEof xs buffer state code = xs

tokenEof xs b s code =
  let newToken = CirruToken (bText b) (bX b) (bY b) (sX s) (sY s) (sPath s)
      newXs = appendItem xs (sLevel s) newToken
  in newXs

indentEof xs buffer state code = xs


escapeNewline xs buffer state code = error "new line while escape"

escapeN :: CirruValue -> CirruBuffer -> CirruState -> String -> CirruValue
escapeN xs b s code =
  let newState = CirruState "string" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer ((bText b) ++ "\n") (bX b) (bY b)
  in parseState xs newBuffer newState (tail code)

escapeT xs b s code =
  let newState = CirruState "string" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer ((bText b) ++ "\t") (bX b) (bY b)
  in parseState xs newBuffer newState (tail code)

escapeElse xs b s code =
  let newState = CirruState "string" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer ((bText b) ++ [head code]) (bX b) (bY b)
  in parseState xs newBuffer newState (tail code)

stringBackslash xs b s code =
  let newState = CirruState "escape" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
  in parseState xs b newState (tail code)

stringNewline xs buffer state code = error "newline in a string"

stringQuote xs b s code =
  let newState = CirruState "token" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
  in parseState xs b newState (tail code)

stringElse xs b s code =
  let newState = CirruState (sName s) ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer ((bText b) ++ [head code]) (bX b) (bY b)
  in parseState xs newBuffer newState (tail code)

spaceSpace xs b s code =
  let newState = CirruState (sName s) ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
  in parseState xs b newState (tail code)

spaceNewline xs b s code =
  if (mod (sNest s) 2) == 1
    then error "incorrect nesting"
    else
      let newState = CirruState "indent" 1 ((sY s)+1) (sLevel s) (sIndent s) 0 (sNest s) (sPath s)
      in parseState xs b newState (tail code)

spaceOpen xs b s code =
  let nesting = createNesting 1
      newXs = appendItem xs (sLevel s) nesting
      newState = CirruState (sName s) ((sX s)+1) (sY s) ((sLevel s)+1) (sIndent s) (sIndented s) ((sNest s)+1) (sPath s)
  in parseState newXs b newState (tail code)

spaceClose xs b s code =
  if ((sNest s) < 1)
    then error "close at space"
    else
      let newState = CirruState (sName s) ((sX s)+1) (sY s) ((sLevel s)-1) (sIndent s) (sIndented s) ((sNest s)-1) (sPath s)
      in parseState xs b newState (tail code)

spaceQuote xs b s code =
  let newState = CirruState "string" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer "" (sX s) (sY s)
  in parseState xs newBuffer newState (tail code)

spaceElse xs b s code =
  let newState = CirruState "token" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer [head code] (sX s) (sY s)
  in parseState xs newBuffer newState (tail code)

tokenSpace xs b s code =
  let newState = CirruState "space" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newToken = CirruToken (bText b) (bX b) (bY b) (sX s) (sY s) (sPath s)
      newXs = appendItem xs (sLevel s) newToken
  in parseState newXs b newState (tail code)

tokenNewline xs b s code =
  let newState = CirruState "indent" 1 ((sY s)+1) (sLevel s) (sIndent s) 0 (sNest s) (sPath s)
      newToken = CirruToken (bText b) (bX b) (bY b) (sX s) (sY s) (sPath s)
      newXs = appendItem xs (sLevel s) newToken
  in parseState newXs b newState (tail code)

tokenOpen xs buffer state code = error "open parenthesis in token"

tokenClose xs b s code =
  let newState = CirruState "space" (sX s) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newToken = CirruToken (bText b) (bX b) (bY b) (sX s) (sY s) (sPath s)
      newXs = appendItem xs (sLevel s) newToken
  in parseState newXs b newState code

tokenQuote xs b s code =
  let newState = CirruState "string" ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
  in parseState xs b newState (tail code)

tokenElse xs b s code =
  let newState = CirruState (sName s) ((sX s)+1) (sY s) (sLevel s) (sIndent s) (sIndented s) (sNest s) (sPath s)
      newBuffer = CirruBuffer ((bText b) ++ [head code]) ((sX s)+1) (sY s)
  in parseState xs newBuffer newState (tail code)

indentSpace xs b s code =
  let newState = CirruState (sName s) ((sX s)+1) (sY s) (sLevel s) (sIndent s) ((sIndented s)+1) (sNest s) (sPath s)
  in parseState xs b newState (tail code)

indentNewline xs b s code =
  let newState = CirruState (sName s) 1 ((sY s)+1) (sLevel s) (sIndent s) 0 (sNest s) (sPath s)
  in parseState xs b newState (tail code)

indentClose xs buffer state code = error "close parenthesis at indent"

indentElse xs b s code =
  if (mod (sIndented s) 2) == 1
    then error ("odd indentation: " ++ (show (sIndented s)))
    else
      let indented = div (sIndented s) 2
          diff = indented - (sIndent s)
          nesting = createNesting 1
          newState = CirruState "space" (sX s) (sY s) ((sLevel s)+diff) indented (sIndented s) (sNest s) (sPath s)
      in
        if diff <= 0
          then
            let newXs = appendItem xs ((sLevel s) + diff - 1) nesting
            in parseState newXs b newState code
        else
          if diff > 0
            then
              let newXs = appendItem xs (sLevel s) nesting
              in parseState newXs b newState code
            else
              parseState xs b newState code
