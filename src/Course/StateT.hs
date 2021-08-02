{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor k of (a produced value `a`, and a resulting state `s`).
newtype StateT s k a =
  StateT {
    runStateT ::
      s
      -> k (a, s)
  }

-- Starting on the 10/7/21

-- | Implement the `Functor` instance for @StateT s k@ given a @Functor k@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor k => Functor (StateT s k) where
  (<$>) :: (a -> b) -> StateT s k a -> StateT s k b
  (<$>) f sa = StateT (\s -> (\(a, s') -> (f a, s')) <$> runStateT sa s)
-- in hindsight this was relatively simple but I needed to do this with a tutorial 
-- i am not sure if this can be implemented in the previous way    
    --StateT (\s -> let (x, s') = (x, s') in  (f <$> x, s'))
--    error "todo: Course.StateT (<$>)#instance (StateT s k)"

--The functor instance has the type signature
--   Pronounced, eff-map.
--  (<$>) :: (a -> b) -> k a -> k b



-- | Implement the `Applicative` instance for @StateT s k@ given a @Monad k@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> runStateT (StateT (\s -> Full ((+2), s ++ (1:.Nil))) <*> (StateT (\s -> Full (2, s ++ (2:.Nil))))) (0:.Nil)
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s ++ (1:.Nil)) :. ((+3), s ++ (1:.Nil)) :. Nil) <*> (StateT (\s -> (2, s ++ (2:.Nil)) :. Nil))) (0:.Nil)
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad k => Applicative (StateT s k) where
  pure :: a -> StateT s k a
  pure a = StateT (\s -> pure (a, s))
  (<*>) :: StateT s k (a -> b) -> StateT s k a -> StateT s k b
  (<*>) sab sa = StateT (\s ->
    (\(ab, s') -> (\(a, s'') -> (ab a, s'')) <$> runStateT sa s') =<< runStateT sab s)

-- this monad was a bit problematic as you have to unwrap both values to 
-- apply the a -> b on the a

-- | Implement the `Monad` instance for @StateT s k@ given a @Monad k@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad k => Monad (StateT s k) where
  (=<<) :: (a -> StateT s k b) -> StateT s k a -> StateT s k b
  (=<<) f sa = StateT (\s -> (\(a, s') -> runStateT (f a) s' ) =<< runStateT  sa s )

--resuming on the 30/7/21
-- still plodding through these. 
--    error "todo: Course.StateT (=<<)#instance (StateT s k)"

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a = StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne ((),1)
state' :: (s -> (a, s)) -> State' s a
state' f = StateT (\s -> ExactlyOne (f s))
--  error "todo: Course.StateT#state'"
-- this was relatively straightforward with hole fits. 

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' sa  = runExactlyOne . runStateT sa --this is the pointfree representaiton

--runState' sa  s = runExactlyOne (runStateT sa s)
--  error "todo: Course.StateT#runState'"

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT :: Functor k => StateT s k a -> s -> k s
execT sa s = snd <$> (runStateT sa s) -- since its a functor, you can map snd across to get the second value
--  error "todo: Course.StateT#execT"

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' :: State' s a -> s -> s
exec' sa =  snd . runState' sa -- now in pointfree

--exec' sa s =  snd (runState' sa s)
--  error "todo: Course.StateT#exec'"

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT :: Functor k => StateT s k a -> s -> k a
evalT sa s = fst <$> (runStateT sa s) -- this is similar to execT
--  error "todo: Course.StateT#evalT"

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' :: State' s a -> s -> a
eval' sa  = fst . runState' sa 
--  error "todo: Course.StateT#eval'"

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: Applicative k => StateT s k s
getT = StateT (\s -> pure (s, s))
-- need to apply pure to put s in the applicative context
--  error "todo: Course.StateT#getT"

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: Applicative k => s -> StateT s k ()
putT x = StateT (\_ -> pure((), x))
--  error "todo: Course.StateT#putT"
-- I am progressing well with StateT, this is a good place to stop

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: Ord a => List a -> List a
distinct' xs =  eval' ( filtering (\x -> state' (\s -> (S.notMember x s, S.insert x s ))) xs ) S.empty
-- this was just a direct grab from State with an update to state'
--  error "todo: Course.StateT#distinct'"
--distinct xs = eval ( filtering (\x -> State (\s -> (S.notMember x s, S.insert x s ))) xs ) S.empty


-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: (Ord a, Num a) => List a -> Optional (List a)
distinctF xs = evalT (filtering 
  (\a -> StateT (\s -> 
    if a > 100 
    then Empty -- as soon as empty is seen the computation short ciruits and returns Empty
    else Full (S.notMember a s, S.insert a s ))) 
  xs) S.empty

-- this is powerful becuase it combines monadic operations, 
-- it combines imperative logic in functional programming.
-- this is like an imperative short circuiting monad.  
-- with StateT we now have a function that has state and optionality

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT k a =
  OptionalT {
    runOptionalT ::
      k (Optional a)
  }

-- this is the optional Monad transformer
-- | Implement the `Functor` instance for `OptionalT k` given a Functor k.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor k => Functor (OptionalT k) where
  (<$>) :: (a -> b) -> OptionalT k a -> OptionalT k b
  (<$>) f oa = OptionalT ((f <$>) <$> runOptionalT oa)

-- stopping here, I am going will, but need to think about the monad transformers a bit more. 
--    error "todo: Course.StateT (<$>)#instance (OptionalT k)"

-- | Implement the `Applicative` instance for `OptionalT k` given a Monad k.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad k => Applicative (OptionalT k) where
  pure :: a -> OptionalT k a
  pure = OptionalT . pure . pure
--    error "todo: Course.StateT pure#instance (OptionalT k)"

  (<*>) :: OptionalT k (a -> b) -> OptionalT k a -> OptionalT k b
  (<*>) oab oa = OptionalT (lift2 (<*>) (runOptionalT oab) (runOptionalT oa))
  --  error "todo: Course.StateT (<*>)#instance (OptionalT k)"
-- this is similar to compose, as we are composing functors 

-- | Implement the `Monad` instance for `OptionalT k` given a Monad k.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad k => Monad (OptionalT k) where
  (=<<) :: (a -> OptionalT k b) -> OptionalT k a -> OptionalT k b
  (=<<) f oa = let
    g (Full a) = 
      runOptionalT (f a)
    g Empty = 
      pure Empty
    in OptionalT (g =<< runOptionalT oa)
     
-- this solution uses pattern matching to define the cases for the bind operatoin
-- i think it is an elegant way to explain what is happening. 
-- the monad for optionalT just checks for something that has either a value or an empty, 
-- if it has an empty it bails out

--    error "todo: Course.StateT (=<<)#instance (OptionalT k)"

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) :: (a -> b) -> Logger l a -> Logger l b
  (<$>) f (Logger ll aa) = Logger  ll (f aa) 
--    error "todo: Course.StateT (<$>)#instance (Logger l)"

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure = Logger Nil 
-- logger is a pair with the first element being a monoid
--    error "todo: Course.StateT pure#instance (Logger l)"

  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  (<*>) (Logger l0 a2b) (Logger l1 a1) = Logger (l0 ++ l1) (a2b a1)
-- this is relatively straightforward

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) :: (a -> Logger l b) -> Logger l a -> Logger l b
  (=<<) alb (Logger l1 a1) = let 
    (Logger l2 b2) = (alb a1) 
    in Logger (l1 ++ l2) b2

-- performing the bind operation manually using the let - in notation
-- this was done un-assisted and with confidence

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 l a = Logger (l :. Nil) a
-- this was easy just create a singleton list manually
--  error "todo: Course.StateT#log1"

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG :: (Integral a, Show a) => List a -> Logger Chars (Optional (List a))
distinctG xs = runOptionalT (evalT (filtering 
  (\a -> StateT (\s -> 
--    if a > 100 
--    then Empty -- as soon as empty is seen the computation short ciruits and returns Empty
--    else (\b -> 
 --     if even b 
  --    then Logger ()Full (S.notMember a s, S.insert a s )))
  --    else  
  OptionalT (
    if a > 100 
    then log1 ("aborting > 100: " ++ show' a) Empty
    else (
      if even a
      then log1 ("even number: " ++ show' a) 
      else pure) (Full (S.notMember a s, S.insert a s )) -- this is inlined to reduce code duplication
  )
    ))
  xs) S.empty)

-- three fuynctions
-- StateT to record State
-- OptionalT to short circuit out with an Empty
-- and Logging
-- this was a very neat solution and I was close to the answer
-- instead of creating a logger type, I just needed to log1 each time it operated
-- I needed to just add an optionalT and keep the eval

--  error "todo: Course.StateT#distinctG"

-- template this based in distintF
-- distinctF xs = evalT (filtering 
--   (\a -> StateT (\s -> 
--     if a > 100 
--     then Empty -- as soon as empty is seen the computation short ciruits and returns Empty
--     else Logger ()Full (S.notMember a s, S.insert a s ))) 
--   xs) S.empty
onFull ::
  Applicative k =>
  (t -> k (Optional a))
  -> Optional t
  -> k (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
