
import Cirru
import CirruParser

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson.Encode.Pretty as Pretty

names = [ "comma"
        , "demo"
        , "folding"
        , "html"
        , "indent"
        , "line"
        , "parentheses"
        , "quote"
        , "spaces"
        , "unfolding"
        ]

testFile :: String -> IO ()
testFile name = do
  let fileName = "../cirru/" ++ name ++ ".cirru"
  let astName = "../ast/" ++ name ++ ".json"
  let config = Pretty.Config {Pretty.confIndent = 2, Pretty.confCompare = mempty}
  code <- readFile fileName
  let cirruData = resolveComma $ resolveDollar (parse code fileName)
  let formated = Pretty.encodePretty' config cirruData
  --putStrLn $ B.unpack formated
  jsonCode <- readFile astName
  --let expectedJson = decode (B.pack jsonCode) :: Maybe CrValue
  --let expected = show expectedJson
  if ((B.unpack formated) == jsonCode)
  then putStrLn "done"
  else do
    putStrLn (B.unpack formated)
    putStrLn "false"

main :: IO ()
main = do
  mapM_ testFile names
