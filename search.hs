import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromJust)
import Text.Printf (printf)


g, h, t :: Double -> Double
g x = x ^ 2 - 9 * x + 8
h x = x + 1 / x
t x = x ^ 4 - 6 * x ^ 2 + 10


partition :: Double -> Double -> Double -> Int -> [(Double, (Double, Double))]
partition a b e n = zip ps intervals
  where
    ps = mark a b e n
    intervals = zip (a:ps) (tail ps ++ [b]) -- second zip, can't fail

mark :: Double -> Double -> Double -> Int -> [Double]
mark a b e n 
    | even n    = 
        let k = fromIntegral $ n `div` 2
            l = (b - a) / (k + 1)
        in  [1..k] >>= (\x -> [a + l * x - e / 2, a + l * x + e / 2])
    | otherwise = 
        let k = fromIntegral n
            l = (b - a) / (k + 1)
        in  fmap (\x -> a + l * x) [1..k] 
 
-- passiveSearch h . partition 0 2 $ 6 
passiveSearch :: Ord b => (a -> b) -> [(a, (a, a))]  -> (a, b, (a, a))
passiveSearch f parts = 
    minimumBy (\(_, l, _) (_, r, _) -> l `compare` r) ys
  where 
    ys = fmap (\(x, int) -> (x, f x, int)) parts

-- dichotomy t 0.1 1 3 8
dichotomy :: (Double -> Double) -> Double -> Double -> Double -> Int
          -> Maybe (Double, Double, (Double, Double))
dichotomy f e a b k
    | odd k || k <= 0 = Nothing
    | otherwise       = go a b (k `div` 2) []
      where 
        go l r n locals
            | n == 0    = Just $
                let m = minimumBy (compare `on` f) (l : r : locals)
                in  (m, f m, (l, r))
            | otherwise = 
                if f1 <= f2 
                    then go l x2 (pred n) (x1 : locals)
                    else go x1 r (pred n) (x2 : locals)                      
          where 
            x1 = (l + r - e) / 2
            x2 = (l + r + e) / 2
            f1 = f x1
            f2 = f x2

fibs :: [Int]
fibs =  1 : 1 : (zipWith (+) <*> tail) fibs
  
  
data Point = InRight | InLeft deriving Show
  
  
fibMethod :: (Double -> Double) -> Double -> Double -> Double -> Int
          -> Maybe (Double, Double, (Double, Double))
fibMethod f e a b n
    | n <= 1    = Nothing
    -- Now, I'm not sure it should be here in there first place 
    -- | e >= (b - a) / (fromIntegral . (fibs !!) . succ $ n) = Nothing
    | otherwise = go a b 1 undefined undefined undefined
      where
        go :: Double -> Double -> Int -> Double -> Double -> Point 
           -> Maybe (Double, Double, (Double, Double))
        go l r j prev fprev {- calc only once -} flag
            -- recurse (n - 1) times
            | j == n = Just (prev, fprev, (l, r))
            | j == 1 =   
                 if f1 <= f2 
                    then go l x2 (succ j) x1 f1 InLeft
                    else go x1 r (succ j) x2 f2 InRight
            | otherwise = 
                case flag of 
                    InLeft  -> 
                        if f1 < fprev {- l ... x1 ... prev ... r -}
                            then go l prev (succ j) x1 f1 InLeft
                            else go x1 r (succ j) prev fprev InRight
                    InRight -> 
                        if fprev < f2 {- l ... prev ... x2 ... r -}
                            then go l x2 (succ j) prev fprev InLeft
                            else go prev r (succ j) x2 f2 InRight 
          where 
            x1 = x (n - j - 1) (-1)
            x2 = x (n - j) (1)
            
            x i s = l + (k i) * (r - l) 
                + s * e * (-1) ^ (nj1 `mod` 2) / fromIntegral (fibs !! nj1)
            k i = fromIntegral (fibs !! i) / fromIntegral (fibs !! nj1)

            f1 = f x1
            f2 = f x2
            
            nj1 = n - j + 1
            
-- Now I just copypasted the last one..     
goldenRatioMethod :: (Double -> Double) -> Double -> Double -> Int
          -> Maybe (Double, Double, (Double, Double))
goldenRatioMethod f a b n
    | n <= 1    = Nothing
    -- won't blow up, all will be set on the first iteration
    | otherwise = go a b 1 undefined undefined undefined
      where
        go :: Double -> Double -> Int -> Double -> Double -> Point 
           -> Maybe (Double, Double, (Double, Double))
        go l r j prev fprev {- calc only once -} flag
            -- recurse (n - 1) times
            | j == n = Just (prev, fprev, (l, r))
            | j == 1 =   
                 if f1 <= f2 
                    then go l x2 (succ j) x1 f1 InLeft
                    else go x1 r (succ j) x2 f2 InRight
            | otherwise = 
                case flag of 
                    InLeft  -> 
                        if f1 < fprev {- l ... x1 ... prev ... r -}
                            then go l prev (succ j) x1 f1 InLeft
                            else go x1 r (succ j) prev fprev InRight
                    InRight -> 
                        if fprev < f2 {- l ... prev ... x2 ... r -}
                            then go l x2 (succ j) prev fprev InLeft
                            else go prev r (succ j) x2 f2 InRight 
          where 
            phi2 = (sqrt 5 - 1) / 2
            phi = (3 - sqrt 5) / 2
               
            x1 = l + phi * (r - l)
            x2 = l + phi2 * (r - l)

            f1 = f x1
            f2 = f x2
            
pretty :: (Double, Double, (Double, Double)) -> String      
pretty (x, y, (z, v)) = printf "%.3f, %.3f (%.3f, %.3f)" x y z v


comparison :: IO () 
comparison = do
    putStr "Passive search N = 16: " 
    putStrLn $ pretty . passiveSearch g . partition 0 8 0.1 $ 16
    putStr "Passive search N = 17: " 
    putStrLn $ pretty . passiveSearch g . partition 0 8 0.1 $ 17
    putStr "Dichotomy:             " 
    putStrLn $ pretty . fromJust . dichotomy g 0.1 0 8 $ 16
    putStr "Fibonacci method:      " 
    putStrLn $ pretty . fromJust . fibMethod g 0.2 0 8 $ 16
    putStr "Golden Ratio method:   " 
    putStrLn $ pretty . fromJust . goldenRatioMethod g 0 8 $ 16