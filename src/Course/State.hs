{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
  runState :: s  -- the current state
            -> (a, s) -- the next state and the output a, a value computed from the current state
          }
-- a way to think of this is that
-- state takes two type variables s a
-- State is really a function (runState) witha constructor around it
-- | Run the `State` seeded with `s` and retrieve the resulting state.
-- input type s to an (a, s)

-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::  State s a -> s -> s
exec (State k ) = snd . k
-- this example just grabs the resultant state after the function is Seeded with s
--  error "todo: Course.State#exec"

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval :: State s a -> s -> a
eval (State k) = fst . k
-- this is similar to exec, however it gets the produced value a
--  error "todo: Course.State#eval"

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get :: State s s
get = State (\x -> (x, x))
-- just apply the function to create a tuple from th input
--get =
--  error "todo: Course.State#get"

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put :: s -> State s ()
put x = State (\_-> ((), x))
--put =
--    error "todo: Course.State#put"

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) :: (a -> b) -> State s a -> State s b
  (<$>) f sa = State (\s -> let (x, s') = (eval sa s, exec sa s) in  (f x, s'))
-- this was not originally workig as it was not changing state as I was passing in s
-- instead of the executed state change of the function
--(<$>) f sa = State (\s -> let (x, s') = (eval sa s, s) in  (f x, s'))

-- i found the way I wrote it makes more sense than the answer
--(<$>) f (State k) =  State (\s -> let (a, t) = k s in (f a, t))
-- I am not sure of the hole fit with k
--

-- finishiing here
-- productive night working alongside this tutorial
--https://www.youtube.com/watch?v=iWNDd4MWtwM&list=PLly9WMAVMrayYo2c-1E_rIRwBXG_FbLBW&index=6
-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> runState (State (\s -> ((+3), s ++ ("apple":.Nil))) <*> State (\s -> (7, s ++ ("banana":.Nil)))) Nil
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s -> (a, s))
  -- just using the lambda function to create the tuple
--    error "todo: Course.State pure#instance (State s)"
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) sf sa = State (\s -> let (x, s') = (eval sa s, exec sf s) in  ((eval sf s) x, (exec sa s')))
  --                             (7, "apple")     7, "apple"               +3 7, ++ banana "apple"
-- similar to fmap but need to extract the function from the state
-- this is done by extracting f in the last tuple by eval sf s
-- and applying this state function to the state as well in exec sf s'
-- I think this is a neat way to build up but potentially hard to read.
-- the answer is very neat, rewriting in the style of the answers
-- State f <*> State a =
--   State (\s -> let (g, t) = f s
--                    (z, u) = a t
--                in (g z, u))


--    error "todo: Course.State (<*>)#instance (State s)"

-- | Implement the `Monad` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
--
-- >>> runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2
-- (10,16)
instance Monad (State s) where
  (=<<) :: (a -> State s b) -> State s a -> State s b
  (=<<) f  sa = State (\s -> let (x, s') = (eval sa s, exec sa s) in runState  (f x) s')
--    error "todo: Course.State (=<<)#instance (State s)"
-- recommencing this course with the Monad instance, its been a few months
--(<$>) :: (a -> b) -> State s a -> State s b
--(<$>) f sa = State (\s -> let (x, s') = (eval sa s, exec sa s) in  (f x, s'))
--using the functor instance.
-- I had a peek at the answers but was on the right track
-- need to revised about type constructors and the like, my memory is a bit scratchy


-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM _ Nil  = pure Empty
findM p (h:.t) = (\x -> if x then pure (Full h) else findM p t)  =<< p h

-- (=<<) ::  (a -> f b)  -> f a -> f b -- monad
-- this was completed with assistance, it can be written with lift2
-- it would be worth reviewing the monadic bind 


-- performing a re-write using find as a basis
--  error "todo: Course.State#findM"

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat :: Ord a => List a -> Optional a
firstRepeat xs = eval (findM (\x -> State (\s -> (S.member x s, S.insert x s ))) xs) S.empty 

-- 1/7/21 -  Find the first element in a List that repeats
-- the idea is to use state as the f in findM, that way state gets passed to the function
-- this solution is using state accumulated and testing the predicate at the same time. 
-- eval is then called (it has been seeded with the empty set to produce an  initial value. )
-- this is a very neat solution


-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct :: Ord a => List a -> List a
distinct xs = eval ( filtering (\x -> State (\s -> (S.notMember x s, S.insert x s ))) xs ) S.empty
-- this is similar to the firstRepeat. 
-- the list is accumulating state based on elements not being a member

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy :: Integer -> Bool
isHappy =
  contains 1 . 
    firstRepeat .
      produce (toInteger . 
                sum . 
                map (join (*). digitToInt) . show')


-- this is an interesting question, it is based on the numbers doing a cycle and using firstRepeat to indicate that the cycle
-- has ended
-- produce maps the function to split the two digits into its squares
-- this function is mapped recursively across items in the list
-- >> produce (toInteger . sum . map (square . digitToInt) . show') 7
--[7,49,97,130,10,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
-- where as
-- >> produce (toInteger . sum . map (square . digitToInt) . show') 4
--[4,16,37,58,89,145,42,20,4,16,37,58,89,145,42,20,4,16,37,58,89,145,42,20,4,16,37,58,89,145,42,20,4,16,3
-- creates an infinite cycle
-- since it terminates at 1, then firstRepeat would return 1 if the number is happy
-- interesting question
-- how does the firstRepeat function know that the infinite list is a cycle? and their won't be a repeat number. 
-- -----------------------------------------
-- -- | Produce an infinite `List` that seeds with the given value at its head,
-- -- then runs the given function for subsequent elements
-- --
-- -- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- -- [0,1,2,3]
-- --
-- -- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- -- [1,2,4,8]
-- produce :: (a -> a) -> a -> List a
-- produce f x = x :. produce f (f x)
-- writing the square function using join (it doubles the argument)
--square :: Int -> Int
--square = join (*)

