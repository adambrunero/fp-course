{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Contravariant

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)
-- a data type compose which takes two type constructors f and g and a type a
--Compose :: (* -> *) -> (* -> *) -> * -> *
-- this is type constructor, type constructor, type , output type
-- in the definithion f could be a list, g could be an optional, a could be Bool
-- the result would be a list of optional books

-- starting the Compose Tutorial 20/7/21
-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  --(<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  (<$>) f (Compose fga) = Compose ((f <$>) <$> fga )
--    error "todo: Course.Compose (<$>)#instance (Compose f g)"
-- this is mapping f over g a to get  g b
-- this is the type signature
-- (<$>) :: (a -> b) -> Compose f g a -> Compose f g b

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
--    error "todo: Course.Compose pure#instance (Compose f g)"
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) (Compose fgab) (Compose fga) = Compose ( lift2 (<*>) fgab fga) -- this is using the sequentional application 
--  (<*>) (Compose fgab) (Compose fga) = Compose (((<*>) <$> fgab) <*> fga -- this is the function for lift2 , FMAP followed by apply)
-- this function maps apply on the g to go from g a to gb
--    error "todo: Course.Compose (<*>)#instance (Compose f g)"
-- (<*>) :: k (a -> b) -> k a -> k b
-- in this case it would be Compose f
-- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b


-- monad can't be done as it is the highest level of specificity, 
--all monads are applicatives, all applicatives are functors, there fore if it could be done the others would need to be implementable too
-- applicative is a superclass of monad, and functor is a superclass of applicative. 
-- if we can implement at least two, then the only one would have to be Monad. 
instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (=<<)#instance (Compose f g)"

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
-- instance (Functor f, Contravariant g) =>
--   Contravariant (Compose f g) where
-- -- Implement the (>$<) function for a Contravariant instance for Compose
-- -- (>$<) :: (b -> a) -> Compose f g a -> Compose f g b
--   (>$<) b2a (Compose fga) = Compose _todo
-- --    error "todo: Course.Compose (>$<)#instance (Compose f g)"
-- -- (>$<) :: Contravariant k => (b -> a) -> k a -> k b

-- this exercise isn't in the answers, not sure if it is possible. 