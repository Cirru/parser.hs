
import Cirru
import Parser
import Text.JSON

fileName = "./cirru/unfolding.cirru"

getJSON :: CirruValue -> JSValue
getJSON (CirruToken text _ _ _ _ _) = JSString (toJSString text)
getJSON (CirruList list) = JSArray $ map getJSON list

main :: IO()
main = do
  code <- readFile fileName
  let json = encode $ getJSON $ resolveComma $ resolveDollar (parse code fileName)
  writeFile "ast/unfolding.json" json
  --putStrLn (show (parse code fileName))
