
import Cirru
import Parser

fileName = "./cirru/unfolding.cirru"

main :: IO()
main = do
  code <- readFile fileName
  putStrLn (show $ resolveComma $ resolveDollar (parse code fileName))
  --putStrLn (show (parse code fileName))
