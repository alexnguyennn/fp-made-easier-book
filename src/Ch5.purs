module Ch5 where

import Prelude (Unit, discard, show)

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)

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
-- applyFlipped x f = f x
applyFlipped = flip apply

infixl 1 applyFlipped as #

test:: Effect Unit
test = do
    log $ show $ flip const 1 2
    flip const 1 2 # show # log