-- From: http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- NOTE: this is likely rather hard to adapt to a 2D case (see blog posts comments)

data U x = U [x] x [x]

right :: U x -> U x
right (U a b (c : cs)) = U (b : a) c cs

left :: U x -> U x
left (U (a : as) b c) = U as a (b : c)

instance Functor U where
  fmap :: (a -> b) -> U a -> U b
  fmap f (U a b c) = U (map f a) (f b) (map f c)

class (Functor w) => Comonad w where
  (=>>) :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

instance Comonad U where
  cojoin :: U a -> U (U a)
  cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)

  coreturn :: U a -> a
  coreturn (U _ b _) = b

rule :: U Bool -> Bool
rule (U (a : _) b (c : _)) = not (a && b && not c || (a == b))

shift :: Int -> U x -> U x
shift i u = iterate (if i < 0 then left else right) u !! abs i

toList :: Int -> Int -> U a -> [a]
toList i j u = take (j - i) $ half $ shift i u
  where
    half (U _ b c) = b : c

main :: IO ()
main =
  let u = U (repeat False) True (repeat False)
   in putStr $
        unlines $
          take 20 $
            map (map (\x -> if x then '#' else ' ') . toList (-20) 20) $
              iterate (=>> rule) u

-- 2D attempt for game of life

data U2 x = U2 (U (U x)) deriving (Show)

instance Functor U2 where
  fmap :: (a -> b) -> U2 a -> U2 b
  fmap f (U2 u) = U2 $ fmap (fmap f) u

instance Comonad U2 where
  coreturn :: U2 a -> a
  coreturn (U2 u) = coreturn (coreturn u)

  cojoin :: U2 a -> U2 (U2 a)
  cojoin (U2 u) = fmap U2 $ U2 $ roll $ roll u
    where
      iterate1 f = tail . iterate f
      roll a = U (iterate1 (fmap left) a) a (iterate1 (fmap right) a)

-- simple update rule for game of life!
rule' :: U2 Bool -> Bool
rule'
  ( U2
      ( U
          (U (u0 : _) u1 (u2 : _) : _) -- top row
          (U (u3 : _) u4 (u5 : _)) -- center row
          (U (u6 : _) u7 (u8 : _) : _) -- bottom row
        )
    ) =
    let n = length $ filter id [u0, u1, u2, u3, u5, u6, u7, u8]
     in u4 && (n == 2 || n == 3) || not u4 && n == 3
