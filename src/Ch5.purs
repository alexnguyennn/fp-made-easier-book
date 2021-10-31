module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (+))

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

