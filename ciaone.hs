halve :: [a] -> ([a],[a])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs) 

third :: [a] -> a
third xs = head (tail (tail xs))

pthird :: [a] -> a
pthird (x:y:z:xs) = z

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail1 :: [a] -> [a]
safetail1 xs | null xs = []
             | otherwise = (tail xs) 

safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 (x:xs) = xs

dio = sum [i**2 | i <- [0..100]]

py :: Int -> [(Int,Int,Int)]
py n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], ((x^2) + (y^2) == (z^2))]

sumdown :: Int -> Int
sumdown 1 = 1
sumdown n = n + sumdown (n-1)

merge :: Ord a => [a] -> [a] -> [a]
merge [] (x:xs) = (x:xs)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) | x<y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f c [] =  c  
foldr' f c (x:xs) = f x (foldr' f c xs)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f c [] = c
foldl' f c (x:xs) = foldl' f (f c x) xs 


{-instance Functor [] where 
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap _ [] = []
    fmap f (x:xs) = f x : fmap f xs 


instance Applicative f where
    -- pure :: a -> [a]
    pure x = f x
    -- <*> :: f (a -> b)  -> f a -> f b
    fx <*> xs = [f x | f <- fx, x <- xs]

instance Monads [] where 
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = [y | x <- xs, y <- f x ]-}

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
    --fmap :: (a -> b) -> ST a -> ST b
    fmap f st = S (\x -> let (y,nst) = app st x in (f y, nst))

instance Applicative ST where
    --pure :: a -> ST a 
    pure x = S (\s -> (x,s))
    -- <*> :: ST (a -> b) -> ST a -> ST b
    sf <*> sx = S (\s -> let (f,s') = app sf s --in app (fmap f sx) s') 
                             (x,s'') = app sx s' in (f x,s''))
            
-- S (s -> (f x,s'')) <*> s


ciao :: ST Int
ciao = S (\s -> (s,s+1))

f :: Int -> Int -> Int
f = \x y -> x*y

instance Monad ST where
    -- (>>=) ST a -> (a -> ST b) -> ST b
    stx >>= f = S (\s -> let (x,s') = app stx s in app (f x) s')



data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show
fresh :: ST Int
fresh = S (\s -> (s,s+1))

alabel :: Tree a -> ST (Tree Int)
{-
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r
-}
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')


alabel (Leaf _ ) = S (\s -> (Leaf s, s+1))
alabel (Node l r) = S (\s -> let (tl,s') = app (alabel l) s
                                 (tr,s'') = app (alabel r) s' in ((Node tl tr),s''))
