{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :: (a -> b) -> Optional a  -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full a) = Full (f a)

-- as the data type has the Patter Full a it can be matched on that for the a
-- this is a functor for Optional type class
--(<$>) :: (a -> b) -> k a -> k b
--  error "todo: Course.Optional#mapOptional"

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty = Empty
bindOptional f (Full a) = f a

-- this is a monadic bind written for the Optional Type clas
--(>>=) :: Monad k => k a -> (a -> k b) -> k b
--  error "todo: Course.Optional#bindOptional"

-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) :: Optional a -> a -> a
(??) x val = case x of
  Full z -> z
  Empty -> val
-- this is just a pattern Match

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) :: Optional a -> Optional a -> Optional a
-- (<+>) x y = case x of
--   Full _ -> x
--   Empty -> case y of
--     Full _ -> y
--     Empty -> Empty
(<+>) (Full x) _ = Full x
(<+>) Empty (Full y) = Full y
(<+>) Empty Empty = Empty
-- using pattern matching is a cleaner solution than nested case statments
-- need to have the Brackets around the Arguments to access the Values

-------------------------------------
-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional :: (a -> b) -> b -> Optional a -> b
optional f _ (Full y) = f y
optional _ b Empty = b
-- this is just an easy pattern match


applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
