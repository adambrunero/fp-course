{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- functor is a superclass of traversable. 

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`

-- deconstructing the type and reconstruct it with the outer type constructor
-- 
class Functor t => Traversable t where
  traverse :: Applicative k => (a -> k b) -> t a -> k (t b)

instance Traversable List where
  traverse :: Applicative k => (a -> k b) -> List a -> k (List b)
  traverse f = foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse :: Applicative k => (a -> k b) -> ExactlyOne a -> k (ExactlyOne b)
  traverse f  a = ExactlyOne <$> ( f $ runExactlyOne a)
-- this is just mapping ExactlyOne over k b
--    error "todo: Course.Traversable traverse#instance ExactlyOne"

instance Traversable Optional where
  traverse :: Applicative k => (a -> k b) -> Optional a -> k (Optional b)
  traverse f oa = optional (\a -> Full <$> f a ) (pure Empty) oa
-- this is like a foldRight on one value
-- an just mapping Full over k b
--    error "todo: Course.Traversable traverse#instance Optional"
--26/7/21 recommencing some exercises. 


-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA :: (Applicative k, Traversable t) => t (k a) -> k (t a) 
--sequenceA tka = traverse id tka
sequenceA = traverse id -- simplified with eta reduction
-- since t is a traversable, it is a good hint to use traverse
--  error "todo: Course.Traversable#sequenceA"

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  traverse f cfga = sequenceA (f <$> cfga)
--        error "todo: Course.Traversable traverse#instance (Compose f g)"
-- this is the type signature
-- (a -> k b) -> Compose f g a -> k (Compose f g b)
-- this instance maps a function of a over (two Traversables f g (composed)) which return an a
-- the k is sequenced out using sequence a

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =  Product (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) f (Product fa ga) = Product (f <$> fa) (f <$> ga)
--    error "todo: Course.Traversable (<$>)#instance (Product f g)"
-- the type signature for functor is
-- (<$>) :: (a -> b) -> Product f g a -> Product f g b
-- to return the product of the data type, just fmap the functor across the two data constructors and return a product type

instance (Traversable f, Traversable g) => Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse f (Product fa ga) = lift2 Product (traverse f fa) (traverse f ga)
--    error "todo: Course.Traversable traverse#instance (Product f g)"
-- traverse :: (a -> k b) -> Product f g a -> k (Product f g b)


-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) =
    error "todo: Course.Traversable (<$>)#instance (Coproduct f g)"

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse =
    error "todo: Course.Traversable traverse#instance (Coproduct f g)"
