
import Cirru
import Parser

fileName = "./cirru/line.cirru"

main :: IO()
main = do
  code <- readFile fileName
  putStrLn (show $ resolveComma (parse code fileName))
