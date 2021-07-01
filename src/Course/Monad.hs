{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative k => Monad k where
  -- Pronounced, bind.
  (=<<) :: (a -> k b) -> k a -> k b

infixr 1 =<<

-- notice the progression of type signatures
-- (<$>) :: (a -> b)    -> f a -> f b  -- fmap
-- (<*>) :: f (a -> b)  -> f a -> f b  -- aplicative
-- (=<<) ::  (a -> f b)  -> f a -> f b -- monad

--monads, applicative and functor are all fuctions
-- there is a heirarchy of generality
-- fmap, applicative and monad
-- these move from generality and speficity
-- but the applications are more restricted as you increase from fmap to monad

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
  (=<<) f a = f $ runExactlyOne a
-- completed this unassisted, looks ok
--    error "todo: Course.Monad (=<<)#instance ExactlyOne"

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) :: (a -> List b) -> List a -> List b
  (=<<) = flatMap
--    error "todo: Course.Monad (=<<)#instance List"
-- this is flatmap
-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) :: (a -> Optional b) -> Optional a -> Optional b
  (=<<) f a = bindOptional f a
  -- 27/7/20
  -- just using bind optional from the Libraries
--  bindOptional _ Empty = Empty
--  bindOptional f (Full a) = f a
--    error "todo: Course.Monad (=<<)#instance Optional"

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  --(=<<) :: (a -> ((->) t b)) -> ((->) t a) -> ((->) t b)
--  (=<<) :: (a -> (t -> b)) -> (t -> a) -> (t -> b)
  (=<<) :: (a -> (t -> b)) -> (t -> a) -> t -> b
-- the brackets were confursing with the type classe
  (=<<) f x t = f (x t) t

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
(<**>) ::  Monad k => k (a -> b) -> k a -> k b
--(<**>) f a = f >>= \f' -> a >>= \a' -> pure (f' a')
-- this seems to be binding to get to the value, then wrapping them at the final stage
-- when working in multiple monadic contexts it can be solved with subsequent lambdas and hole exploration
--this can be re-written in do notation
(<**>) f a = do f' <- f
                a' <- a
                return (f' a')
-- pure is not require as this is handled by the do notation.
-- I find the do notation more elegant
-- this is the 'I am applicative, destroyer of minds' example
-- ((*) <**> (+1)) x  becoming x^2 + 1

-- error "todo: Course.Monad#(<**>)"
-- this is the sequential operator
infixl 4 <**>

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join :: Monad k => k (k a) -> k a
join x = (=<<) id x
-- might need to review this join function
--  (=<<) :: (a -> k b) -> k a -> k b
--https://stackoverflow.com/questions/3382210/monad-join-function
--  error "todo: Course.Monad#join"
-- note the join function is reasonably important in monads
-- it can describe bind as
-- (=<<) m f = join (fmap f m)
-- https://www.ahnfelt.net/monads-forget-about-bind/

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) :: Monad k => k a -> (a -> k b) -> k b
(>>=) = flip (=<<)
  --error "todo: Course.Monad#(>>=)"
-- monads support bind which is an additional constraint
infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, Kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) :: Monad k => (b -> k c) -> (a -> k b) -> a -> k c
(<=<) f g a = (=<<) f (g a)

--(>>=) :: k x -> (x -> k y) -> ky

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
