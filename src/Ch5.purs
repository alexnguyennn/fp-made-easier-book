module Ch5 where

import Data.Boolean (otherwise)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, show, (+), (/=), (<), (==), (>), (>=))

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
-}
reverse :: List ~> List 
reverse l =  go l Nil where
    go Nil rl = rl
    go (x : xs) rl = go xs (x : rl)


{-
    todo: writing on derived
    todo: explain formative case
        concat (lh : (lsh : Nil)) = go lh lsh where
            go Nil listToConcat  = listToConcat
            go (x : xs) listToConcat = x : (go xs listToConcat)
-}
concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (l : ls) = go l (concat ls) where 
    go Nil listToConcat  = listToConcat
    go (x : xs) listToConcat = x : (go xs listToConcat)

{-
    todo: implement alternative concat implementation from book
-}

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
    -- log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil) 

