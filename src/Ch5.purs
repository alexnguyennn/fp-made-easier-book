module Ch5 where

import Data.Boolean (otherwise)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, max, negate, otherwise, show, (+), (-), (/=), (<), (<<<), (<=), (==), (>), (>=))

-- import Data.List (singleton)

-- flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
-- flip f x y = f y x
flip :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip f = \x y -> f y x

-- flip :: ∀ a b c. (a -> b -> c) -> b -> (a -> c) 
-- flip f x = \y -> f y x

-- It can also be thought of as creating a function that ignores its argument
-- useful to use partial application to 'freeze' a predetermined value that will be returned regardless of next input
const :: ∀ a b . a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply
-- applyFlipped x f = f x

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
-- singleton x = Cons x Nil
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
-- snoc Nil x = x : Nil
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: ∀ a. List a -> Int
-- non tail recursive implementation - can stack overflow
-- tail recursive -> recursion is last operation so can be smart and recurse w/o stack space
-- length Nil = 0
-- length (_ : xs) = 1 + length xs
length l = go 0 l where
    go :: Int -> List a -> Int
    go acc Nil = acc
    go acc (_ : xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs


last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x -- must come before general case, otherwise will skip
last (_ : xs) = last xs
-- alternative: last (_ : xs) = if length xs == 1 then head xs else last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
-- init (x : xs) = x : init xs
init l = Just $ go l where
    go Nil = Nil
    go (_ : Nil) = Nil
    go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ idx | idx < 0 = Nothing
index l idx = go l idx 0 where
    go :: List a -> Int -> Int -> Maybe a
    go Nil _ _ = Nothing
    go (x : xs) i j | j == i    =  Just x
                    | j > i     =  Nothing
                    | otherwise =  go xs i (j + 1)

-- alternate impl: - start highest and recurse backward
-- index Nil _ = Nothing
-- index _ idx | idx < 0 = Nothing
-- index (x : _ ) 0 = Just x -- uses 0 count to terminate
-- index (_ : xs ) i = index xs (i - 1) 
infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred l = go l 0 where
    go Nil _ = Nothing
    go (x : xs) idx = 
        if pred x 
        then Just idx 
        else go xs (idx + 1)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
-- findLastIndex pred l = go l 0 (-1) where
    -- go Nil _ latest | latest < 0 = Nothing
                    -- | otherwise = latest
    -- go (x : xs) idx latest = 
    --     if pred x 
    --     then go xs (idx + 1) idx
    --     else go xs (idx + 1) latest
findLastIndex pred l = go l 0 Nothing where
    -- note: remove ∀ from signature to prevent clobbering global a in type signature above
    go :: List a -> Int -> Maybe Int -> Maybe Int 
    go Nil _ latest = latest
    go (x : xs) idx latest = 
        go xs (idx + 1) (if pred x then Just idx else latest) 


{- 
    ~> is natural transformation (something to do with functors)
    -- equivalent to: 
    reverse :: ∀ a. List a -> List a
    -- ~> is a type operator; binary operator on types, not values
    implementation strategy:
    * create new list -> nil stub as input
    * iterate through, add to new list as we go
    * nil case - return new list

    learning:
    * using separate data structure in fp means inject it as input (kinda like di)
    * hard to initialise DS during recursion

    tail recursive implementation; only does recursing, no other calls
-}
reverse :: List ~> List 
reverse l =  go l Nil where
    go Nil rl = rl
    go (x : xs) rl = go xs (x : rl)


{-
    own solution: design helper function that concatted between 2 lists first,
    hence the (lh: (lsh : Nil)) case below.
    then recurse through list of lists (ls) normally and call go on it with rest of list each time instead
        concat (lh : (lsh : Nil)) = go lh lsh where
            go Nil listToConcat  = listToConcat
            go (x : xs) listToConcat = x : (go xs listToConcat)
-}
concat :: ∀ a. List (List a) -> List a
-- concat Nil = Nil
-- concat (l : ls) = go l (concat ls) where 
--     go Nil listToConcat  = listToConcat
--     go (x : xs) listToConcat = x : (go xs listToConcat)

concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
-- filter f (x : xs) = if f x then x : filter f xs else filter f xs
{-
    alternate implementation with guards
filter f (x : xs) | f x = x : filter f xs
                  | otherwise = filter f xs
-}
-- tail recursive solution (make sure reverse is tail recursive too)
--  slower (have to iterate through again to reverse) but takes less space on stack
-- filter f l = reverse $ go Nil l where
filter f = reverse <<< go Nil where
    go nl Nil = nl
    go nl (x : xs) | f x = go (x : nl) xs
    go nl (_ : xs) | otherwise = go nl xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
-- catMaybes ((Just val) : xs) = val : catMaybes xs
-- catMaybes ( Nothing : xs) = catMaybes xs
-- use case:
catMaybes (x : xs) = case x of
    Just y -> y : catMaybes xs
    Nothing -> catMaybes xs

range :: Int -> Int -> List Int
-- range start end =  go start end where
--     go :: Int -> Int -> List Int
--     go cur end | cur == end = end : Nil
--     go cur end | start < end = cur : go (cur + 1) end
--     go cur end | start > end = cur : go (cur - 1) end

-- book soln; don't be too eager to break out the subfunction? go had a missing case..
-- range start end | start == end = singleton start
--                 | otherwise = 
--                     let step = if  start < end then 1 else (-1) in
--                         start : range (start + step) end

-- NOTE: let in this case is part of recursive call so will be recalculated every time
-- range start end =
--     let step = if  start < end then 1 else (-1) in
--     if start == end 
--     then singleton start
--     else start : range (start + step) end 

-- optimise range - do let only once
-- NOTE: variables from in scope terminates at where - can't pass through to go
-- range start end = 
--     let step = if  start < end then 1 else (-1) in
--         go start step where
--             -- go start' step = 
--             --     if start' == end 
--             --     then singleton start'
--             --     else start' : range (start' + step) end  
--             -- or
--             go start' step | start' == end = singleton start'
--             go start' step | otherwise = start' : go (start' + step) end  

-- range start end = go start where
--     go start' | start' == end = singleton start'
--     go start' | otherwise = start' : go (start' + step) end  
--     step = if  start < end then 1 else (-1) 

-- backwards
-- range start end = go Nil start end where 
--     go rl start' end' | start' == end' = start': rl
--                       | otherwise = go (start': rl) (start' + step) end'
--     step = if  start < end then 1 else (-1) 
-- note: also flipping step logic as it's still in terms of original start/end adn we're flipping start/end order 
-- to account for backward recursing
range start end = go Nil end start where 
    go rl start' end' | start' == end' = start': rl
                 | otherwise = go (start': rl) (start' + step) end'
    step = if  start < end then (-1) else 1 


take :: ∀ a. Int -> List a -> List a
-- take _ Nil = Nil
-- take 0 _ = Nil
-- take n (x : xs) = x : take (n - 1) xs


-- tail recursive
-- building list in tail recursive manner usually reverses order of final list - need to deal with this when optimising
-- fix weakness with negative numbers where infinite recurse with max
take n = reverse <<< go Nil (max 0 n) where
    go nl _ Nil = nl
    go nl 0 _ = nl
    go nl n' (x : xs) = go (x : nl) (n' -1) xs


drop :: ∀ a. Int -> List a -> List a
drop n l = go (max 0 n) l where
    go _ Nil = Nil
    go 0 l' = l'
    go n' (_ : xs) = go (n' -1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) | pred x = x : takeWhile pred xs
takeWhile _ _ | otherwise = Nil

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred (x : xs) | pred x = dropWhile pred xs
dropWhile _ l | otherwise = l
-- note: l@(x: xs) allows reference to both full list and destructure

takeEnd :: ∀ a. Int -> List a -> List a 
-- or this implementation (but double traversal)
--takeEnd n l = drop (max 0 $ length l - n) l 
-- takeEnd n l =  go Nil n (reverse l) where
--     go tl _ Nil = tl
--     go tl 0 _ = tl
--     go tl n' (x : xs) = go (x : tl) (n' - 1) xs

takeEnd n l = snd (go l) where
    -- go :: Tuple Int List a -> Tuple Int List a
    go Nil = Tuple 0 Nil
    -- self implementation
    -- go (x : xs) = 
    --     let 
    --         next = go xs
    --         iterator = fst next
    --         taken = snd next
    --     in
    --         Tuple (iterator + 1) (
    --             if iterator < n then
    --                 x : taken
    --             else
    --                 taken)
    {-
    -- book implementation
    NOTE: it's cleaner because it makes use of applyFlipped
    (#) operator; an a -> (a -> b) -> b
    This is useful to specify some operation to generate an a
    before doing something with it (supplying lambda a->b) - result will actually apply to yield the b
    Pattern matching uses Type and assign param to each part
    -}
    -- go (x : xs) = go xs 
    --     # \(Tuple c nl) -> 
    --         Tuple (c + 1) $ if c < n then x : nl else nl
    -- this implementation increments c even if not adding to nl
    -- this is fine since value is discarded at top
    -- alternate impl that will not incr c if not adding to nl
    go (x : xs) = go xs 
        # \tup@(Tuple c nl) -> 
            if c < n then Tuple (c + 1) (x : nl) else tup


test:: Effect Unit
test = do
    log $ show $ flip const 1 2
    flip const 1 2 # show # log
    log $ show $ singleton "xyz"
    log $ show $ null Nil
    log $ show $ null ("abc" : Nil)
    log $ show $ snoc (1 : 2 : Nil) 3
    log $ show $ length $ 1 : 2 : 3 : Nil
    -- log $ show $ (head Nil :: Maybe Unit)
    log $ show $ head (Nil :: List Unit) -- explicitly show return type of Nil list (since no a to infer type) - use unit to show type doesn't matter on show
    log $ show $ head ("abc" : "123" : Nil)
    log $ show $ (tail Nil :: Maybe (List Unit))
    log $ show $ tail ("abc" : "123" : Nil)
    log $ show $ (last Nil :: Maybe Unit)
    log $ show $ last ("a" : "b" : "c" : Nil)
    log $ show $ init (Nil  :: List Unit)
    log $ show $ init (1: Nil)
    log $ show $ init (1 : 2 : Nil)
    log $ show $ init (1 : 2 : 3 : Nil)
    log $ show $ uncons (1 : 2 : 3 : Nil)
    log $ show $ index (1: Nil) 4
    log $ show $ index (1 : 2 : 3 : Nil) 1
    log $ show $ index (Nil :: List Unit) 0
    log $ show $ index (1 : 2 : 3 : Nil) (-99)
    log $ show $ (1 : 2 : 3 : Nil) !! 1
    log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil) 
    log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil) 
    log $ show $ findIndex (10 /= _) (Nil :: List Int) 
    log $ show $ findLastIndex (_ == 10) (Nil :: List Int) 
    log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil) 
    log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
    log $ show $ reverse (10 : 20 : 30 : Nil)
    log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
    log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil) 
    log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
    log $ show $ range 1 10
    log $ show $ range 3 (-3)
    log $ show $ take 5 (12 : 13 : 14 : Nil)
    log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
    log $ show $ take (-5) (12 : 13 : 14 : Nil)
    log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
    log $ show $ drop 10 (Nil :: List Unit)
    log $ show $ drop (-20) (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
    log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
    log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
    log $ show $ takeWhile (_ <= 2) (1 : 2 : 3 : Nil)
    log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
    log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil) 
    log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
    log $ show $ takeEnd 10 (1 : Nil)


