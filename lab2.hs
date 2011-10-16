import Data.Bits

main = print $ show (hilbert 9 (9, 3))

-- | convert (x,y) to d
-- As described in http://blog.notdot.net/2009/11/Damn-Cool-Algorithms-Spatial-indexing-with-Quadtrees-and-Hilbert-Curves
hilbert :: Int -> (Int, Int) -> Int
hilbert n p = hilbert0 p 'a' 0 (n-1)

hilbert0 :: (Num a) => (Int, Int) -> Char -> a -> Int -> a
hilbert0 _     _  d (-1) = d
hilbert0 (x,y) sq d i    = hilbert0 (x,y) (snd hm) ((fst hm) + d * 4) (i - 1)
                             where qx = if (x `testBit` i) then 1 else 0
                                   qy = if (y `testBit` i) then 1 else 0
                                   hm = hilbert_map sq qx qy 

hilbert_map 'a' 0 0 = (0, 'd')
hilbert_map 'a' 0 1 = (1, 'a')
hilbert_map 'a' 1 0 = (3, 'b')
hilbert_map 'a' 1 1 = (2, 'a')
hilbert_map 'b' 0 0 = (2, 'b')
hilbert_map 'b' 0 1 = (1, 'b')
hilbert_map 'b' 1 0 = (3, 'a')
hilbert_map 'b' 1 1 = (0, 'c')
hilbert_map 'c' 0 0 = (2, 'c')
hilbert_map 'c' 0 1 = (3, 'd')
hilbert_map 'c' 1 0 = (1, 'c')
hilbert_map 'c' 1 1 = (0, 'b')
hilbert_map 'd' 0 0 = (0, 'a')
hilbert_map 'd' 0 1 = (3, 'c')
hilbert_map 'd' 1 0 = (1, 'd')
hilbert_map 'd' 1 1 = (2, 'd')
