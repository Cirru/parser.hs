
import Cirru
import Parser

fileName = "./cirru/spaces.cirru"

main :: IO()
main = do
  code <- readFile fileName
  putStrLn (show $ parse code fileName)
