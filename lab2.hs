import Data.Bits

main = print $ show (hilbert 9 (9, 4))

-- | convert (x,y) to d
-- As described in http://blog.notdot.net/2009/11/Damn-Cool-Algorithms-Spatial-indexing-with-Quadtrees-and-Hilbert-Curves
hilbert :: Int -> (Int, Int) -> Int
hilbert n p = hilbert0 p 'a' 0 (n-1)

hilbert0 :: (Num a) => (Int, Int) -> Char -> a -> Int -> a
hilbert0 _     _  d (-1) = d
hilbert0 (x,y) sq d i    = hilbert0 (x,y) (snd hm) (fst hm + d * 4) (i - 1)
                             where qx = x `intTestBit` i
                                   qy = y `intTestBit` i
                                   hm = hilbertMap sq qx qy 

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
