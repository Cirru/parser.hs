
import Cirru

emptyCirruList :: CirruValue
emptyCirruList = CirruList []

a :: CirruValue
a = CirruString "demo"

b :: CirruValue
b = CirruList []

c :: CirruValue
c = appendItem emptyCirruList 0 b

d :: CirruValue
d = appendItem c 1 a

main :: IO()
main = do
  putStrLn (show $ c)
  putStrLn (show $ d)