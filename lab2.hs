import Data.Bits
import Data.Ix
import Data.List 
import Data.Function 
import Control.Monad (liftM2)

main = putStrLn $ hilbertCurve 6

cartProd :: [a] -> [b] -> [(a, b)]
cartProd = liftM2 (,)

-- | Sample function used to draw Hilbert curve in Excel of any other spreadsheet
hilbertCurve n = concatMap (\a -> show (fst a) ++ "\t" ++ show (snd a) ++ "\n") 
                           (map snd (sortBy (compare `on` fst) (map (\k -> (hilbert n k, k)) (cartProd r r))))
                    where r = range (0, 2 ^ n - 1)

-- | convert (x,y) to d
-- As described in http://blog.notdot.net/2009/11/Damn-Cool-Algorithms-Spatial-indexing-with-Quadtrees-and-Hilbert-Curves
hilbert :: (Num a) => Int -> (Int, Int) -> a
hilbert n (x, y) = hilbert0 'a' 0 (n-1)
              where hilbert0 _  d (-1) = d
                    hilbert0 sq d i    = hilbert0 sq' (d' + d * 4) (i - 1)
                                                 where qx = x `intTestBit` i
                                                       qy = y `intTestBit` i
                                                       (d', sq') = hilbertMap sq qx qy 

intTestBit :: Bits a => a -> Int -> Int
intTestBit x i = if x `testBit` i then 1 else 0

hilbertMap 'a' 0 0 = (0, 'd')
hilbertMap 'a' 0 1 = (1, 'a')
hilbertMap 'a' 1 0 = (3, 'b')
hilbertMap 'a' 1 1 = (2, 'a')
hilbertMap 'b' 0 0 = (2, 'b')
hilbertMap 'b' 0 1 = (1, 'b')
hilbertMap 'b' 1 0 = (3, 'a')
hilbertMap 'b' 1 1 = (0, 'c')
hilbertMap 'c' 0 0 = (2, 'c')
hilbertMap 'c' 0 1 = (3, 'd')
hilbertMap 'c' 1 0 = (1, 'c')
hilbertMap 'c' 1 1 = (0, 'b')
hilbertMap 'd' 0 0 = (0, 'a')
hilbertMap 'd' 0 1 = (3, 'c')
hilbertMap 'd' 1 0 = (1, 'd')
hilbertMap 'd' 1 1 = (2, 'd')
