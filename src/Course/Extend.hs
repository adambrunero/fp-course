{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Extend where

import Course.Core
import Course.ExactlyOne
import Course.List
import Course.Optional
import Course.Functor

-- | All instances of the `Extend` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g. (f <<=) . (g <<=) ≅ (<<=) (f . (g <<=))`
class Functor k => Extend k where
  -- Pronounced, extend.
  (<<=) ::
    (k a -> b)
    -> k a
    -> k b

infixr 1 <<=

-- 3/8/21- starting unassisted on the Extend type class

-- | Implement the @Extend@ instance for @ExactlyOne@.
--
-- >>> id <<= ExactlyOne 7
-- ExactlyOne (ExactlyOne 7)
instance Extend ExactlyOne where
  (<<=) :: (ExactlyOne a -> b) -> ExactlyOne a -> ExactlyOne b
  (<<=) f a = ExactlyOne $ f a 
-- this was straightforward. 
--    error "todo: Course.Extend (<<=)#instance ExactlyOne"

-- | Implement the @Extend@ instance for @List@.
--
-- >>> length <<= ('a' :. 'b' :. 'c' :. Nil)
-- [3,2,1]
--
-- >>> id <<= (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)
-- [[[4,5,6],[1,2,3]],[[4,5,6]]]
instance Extend List where
  (<<=) :: (List a -> b) -> List a -> List b
  (<<=) lab (x :. xs) = lab (x :. xs) :. (lab <<= xs) 
  (<<=) _ Nil = Nil
-- this was the same as the answers and worked out unassisted. 
--    error "todo: Course.Extend (<<=)#instance List"

-- | Implement the @Extend@ instance for @Optional@.
--
-- >>> id <<= (Full 7)
-- Full (Full 7)
--
-- >>> id <<= Empty
-- Empty
instance Extend Optional where
  (<<=) :: (Optional a -> b) -> Optional a -> Optional b
  (<<=) _ Empty =  Empty 
  (<<=) oab oa =  Full (oab oa) 
-- I found the pattern match answer easier than the solutions
-- which was (oab . Full) <$> oa  
-- this relies on the fact that Full <$> Empty is equal to empty
--    error "todo: Course.Extend (<<=)#instance Optional"

-- | Duplicate the functor using extension.
--
-- >>> cojoin (ExactlyOne 7)
-- ExactlyOne (ExactlyOne 7)
--
-- >>> cojoin (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> cojoin (Full 7)
-- Full (Full 7)
--
-- >>> cojoin Empty
-- Empty
cojoin :: Extend k => k a -> k (k a)
cojoin = (<<=) id 
-- I peeked at the answers for this which was a shame
-- the answer was given in the solutions of the previous questions
-- this is a sign I am getting tired. 
-- time to pack it in for the night 
--  error "todo: Course.Extend#cojoin"
