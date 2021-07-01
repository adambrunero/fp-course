{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo: ...") with an appropriate
--   solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import Course.Optional
import qualified System.Environment as E
import qualified Prelude as P
import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap ((P.foldr (:.) Nil) :: ([a] -> List a)) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List t =
  Nil
  | t :. List t -- cons is an infix operator
  deriving (Eq, Ord)

-- there are two constructors, number of pipes + 1
-- this is a recursive data type as the definition refers to itself
-- Right-associative - this is a definition, the 5 is a bind heirarchy (fixity)

infixr 5 :.
--eg
-- 44 :. 55 :. Nil
-- [44,55]
-- new function
-- add 10 to the head of the list
exampleListProblem :: List Integer -> List Integer
exampleListProblem Nil = Nil
exampleListProblem (h :. t) = h + 10 :. t


instance Show t => Show (List t) where
  show = show . hlist

-- The list of integers from zero to infinity.
infinity :: List Integer
infinity = let inf x = x :. inf (x+1) in inf 0

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

-- a foldRight is performed for list constructor replacment
-- foldRight f z list replaces in list
-- every occurence of Cons (:.) with f
-- any occurence of Nil  with z

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

--takes three arguments
--  f (b -> a -> b) - function from b and a to b
-- z is the b, what values to start the loop at
--  list the list to operate on
--does this look
-- \f z list ->
-- var r = x
-- foreach (a in list)
--    r = f(r, a)
-- return r
-- foldleft is used for accumulation parameters,
-- it apples the fuynction to the first element then 'sequences' in that value
-- to the function applied over the tail so that the accumulation is done on the fly

-- * redo all the functions with foldleft and foldright at a later stage


-- END Helper functions and data types

-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> \x -> x `headOr` infinity == 0
--
-- prop> \x -> x `headOr` Nil == x
headOr :: a -> List a -> a
headOr a Nil = a
headOr _ (x:._) = x


-- with some eta reduction
--headOr = foldRight const
--  error "todo: Course.List#headOr"

--headOr' :: List a -> a this is called an uninhabited type, there is no solution

-- | The product of the elements of a list.
--
-- >>> product Nil
-- 1
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
product :: List Int -> Int
--using the rudimentary method to explore the function
product =
  \x -> case x of  -- lambda x needs a list which is either Nil or t :. List t
    Nil -> 1
    h :. t -> h * product t

--this is a patter matching solution
--product Nil = 1
--product (x:.xs) = x * product xs

--18/7/20 - rewritten as newproduct with foldLeft
newproduct :: List Int -> Int
--newproduct = \x -> foldLeft (*) 1 x
-- can simplify this with eta reduction
newproduct = foldLeft (*) 1

-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> \x -> foldLeft (-) (sum x) x == 0
sum :: List Int -> Int
sum = \x -> case x of
  Nil -> 0
  h :. t -> h + sum t
-- this is a pattern matching solution
--sum Nil = 0
--sum (x:.xs) = x + sum xs
-- another solution using foldLeft
--sum list = foldLeft (\a -> \b -> a + b) 0 list
-- eta reduction
-- \x -> f x -- whenever i see this
-- f -- i can do this
--sum list = foldLeft (\a -> \b -> a + b) 0 list
--sum list = foldLeft (\a -> \b -> (+) a b) 0 list
--sum list = foldLeft (\a -> \b -> ((+) a) b) 0 list
--sum list = foldLeft (\a -> ((+) a) ) 0 list --remove b
--sum list = foldLeft (\a -> ((+) a)) 0 list
--
--  \ _ -> x
-- const
-- 18/7/20 rewritten with foldleft
newsum :: List Int -> Int
newsum = foldLeft (+) 0


-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
--
-- prop> \x -> sum (map (const 1) x) == length x
length :: List a -> Int
-- length Nil = 0
-- length (x:.xs) = 1 + length xs

-- a solution using foldLeft
--length  = foldLeft (\r -> const((+) 1 r)) 0
length  = foldLeft (const . (+) 1) 0
-- const :: a-> b-> a

--function compostion
-- \x -> f (g x)
-- f . g
-- point full reverses pointfree form

----------------------------------
-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> \x -> headOr x (map (+1) infinity) == 1
--
-- prop> \x -> map id x == x
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (x:.xs) = f x :. map f xs
-- you can break this down into typ signatures
-- this is a revision exercise
--map = \f -> foldRight ((:.) .f) Nil
testmap :: (a -> b) -> List a -> List b
testmap = \f -> foldRight ((:.).f) Nil

-- λ> :t \f -> ((:.).f)
--   \f -> ((:.).f) :: (a -> t) -> a -> List t -> List t
----------------------------------
-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> \x -> headOr x (filter (const True) infinity) == 0
--
-- prop> \x -> filter (const True) x == x
--
-- prop> \x -> filter (const False) x == Nil
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
-- filter f (x:.xs) = case (f x) of
--   True -> x :. (filter f xs)
--   False -> filter f xs
-- this is using case statements
-- the next line is using if else statments
filter f (x:.xs) = if f x then x :. filter f xs else filter f xs
-- this can be re written with let notation
-- filter p (h:.t) =
--   let c = filter p t
--   in bool id (h:.) (p h) c
--day 2 rewritten with foldRIght
--filter = \p -> foldRight (\h t -> if p then h :. t else t) Nil
-- fold right does constructor replacement
--foldright :: (a-> b-> b) -> b -> List a -> b
-- (a-> b -> b) is a function, accumulated partial result
-- b is base value (list is Nil)
-- this is non strict evaluation
-- foldright example

newmap :: (a -> t) -> List a -> List t
-- newmap f = \list -> foldRight (\h t -> f h :. t ) Nil list
-- newmap f = \list -> foldRight (\h t -> (:.) (f h) t ) Nil list -- now perform eta reduction
-- newmap f = foldRight (\h -> (:.) (f h) ) Nil -- eta reduction again
-- newmap f = foldRight ((.) (:.) f ) Nil -- put infix in

newmap f = foldRight ((:.) . f ) Nil

-- read dot as right to left as 'and then'
-- you can write every list function with foldright

----------------------------------
-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> \x -> headOr x (Nil ++ infinity) == 0
--
-- prop> \x -> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> \x -> x ++ Nil == x
(++) :: List a -> List a -> List a
-- (++) x Nil = x
-- (++) Nil y = y
-- (++) (x:.xs) y = x :. (xs ++ y)

(++) = \x y -> foldRight (:.) y x
-- foldright replace cons with cons and Nil with y
-- (++) x y  =
--   case x of
--     Nil -> y
-- concise example for later (++) flip (foldRight (:.))
infixr 5 ++


----------------------------------
-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> sum (map length x) == length (flatten x)

-- functions over List that you may consider using
-- foldRight :: (a -> b -> b) -> b -> List a -> b
-- foldRight _ b Nil      = b
-- foldRight f b (h :. t) = f h (foldRight f b t)


flatten :: List (List a) -> List a
flatten Nil = Nil
flatten (x:.xs) = x ++ (flatten xs)
-- this is just one layer of abtraction up, just calling append on lists

--flatten = \list -> foldRight (\x y -> x ++ y) Nil list
-- this is appending lists from the right with the Nil list as a unit element
-- [[1, 2, 3], [4, 5, 6], [9]]
--  [1, 2, 3] + [4, 5, 6] + [9] + []
-- fold right does constructor replacement
--foldright :: (a-> b-> b) -> b -> List a -> b
-- (a-> b -> b) is a function, accumulated partial result
-- b is base value (list is Nil)
-- this is non strict evaluation
-- foldright example
--18/7/20 new flatten example
newflatten :: List (List a) -> List a
newflatten = \list -> foldRight (++) Nil list

----------------------------------
-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> \x -> flatMap id (x :: List (List Int)) == flatten x
flatMap :: (a -> List b) -> List a -> List b
-- flatMap  _ Nil = Nil
-- flatMap f (h:.t) = f h ++ flatMap f t
-- the pattern match solution is good
flatMap f = flatten . map f
-- this is a more elegant solution
-- this can be rewritten with foldright
--flatMap f = foldRight (\h t -> f h ++ t) Nil

--flatMap = \f -> foldRight ((++) . f) Nil


----------------------------------
-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x
flattenAgain :: List (List a) -> List a
--flattenAgain = flatMap (\x -> id x)
flattenAgain = flatMap id

-- this is just using the identity function to map List a -> Lista
-- since this is just a flattening with no function applied, id is the appropriate function.


-----------------------------------------
-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values,
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional :: List (Optional a) -> Optional (List a)
--seqOptional (h :. _ ) =
--seqOptional = \list -> flatMap mapOptional list
seqOptional =   foldRight (twiceOptional (:.)) (Full Nil)
--18/7/20, this was a copy from the answers,
-- need to think of a way to understand this.
--  error "todo: Course.List#seqOptional"

-----------------------------------------
-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find :: (a -> Bool) -> List a -> Optional a
-- find f Nil = Empty
-- find f (x:.xs) = case f x of
--   True -> Full x
--   False -> find f xs
-- this was relatively straightforward
-- I am sure this could be re-written with filter rather than stepping through elements
find p x = case filter p x of
  Nil -> Empty
  h:._ -> Full h

-- this is a more elegant solution, review the list after filter is applied
-- if there is a list return a head.


-----------------------------------------
-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 :: List a -> Bool
--lengthGT4 x = length x > 4
-- this doesn't work as you cant find the lenght of an infinite list
-- resorting to pattern matching.
lengthGT4 (_:._:._:._:._:._) = True
lengthGT4 _ = False
-- easiest to pattern match


-----------------------------------------
-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse :: List a -> List a
--reverse (x:.xs) = reverse xs ++ (x:.Nil) -- this needs a single element list
--this is a slow solution, use a helper function
reverse x = reverse0 Nil x
--this function steps through the list adding the head of the list to the head of the accumulator
-- starting with Nil to get a correct list the first pattern match returns the reversed list
reverse0 :: List a -> List a -> List a
reverse0 acc Nil = acc
reverse0 acc (h:.t) = reverse0 (h:.acc) t
--flip cons (r, el) = el :. r
-- reverse list  = foldLeft (\r el -> el :. r) Nil list
--reverse = foldLeft (flip (:.)) Nil

-----------------------------------------
-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce :: (a -> a) -> a -> List a
produce f x = x :. produce f (f x)

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> \x y -> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> \x -> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse = reverse
-- need to check over the description in the answers.
  --error "todo: Is it even possible?"

---- End of list exercises

largeList ::
  List Int
largeList =
  listh [1..50000]

hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

listh ::
  [a]
  -> List a
listh =
  P.foldr (:.) Nil

putStr ::
  Chars
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  Chars
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  FilePath
  -> IO Chars
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  FilePath
  -> Chars
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

getLine ::
  IO Chars
getLine =
  P.fmap listh P.getLine

getArgs ::
  IO (List Chars)
getArgs =
  P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf ::
  Eq a =>
  List a
  -> List a
  -> Bool
isPrefixOf Nil _ =
  True
isPrefixOf _  Nil =
  False
isPrefixOf (x:.xs) (y:.ys) =
  x == y && isPrefixOf xs ys

isEmpty ::
  List a
  -> Bool
isEmpty Nil =
  True
isEmpty (_:._) =
  False

span ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
span p x =
  (takeWhile p x, dropWhile p x)

break ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
break p =
  span (not . p)

dropWhile ::
  (a -> Bool)
  -> List a
  -> List a
dropWhile _ Nil =
  Nil
dropWhile p xs@(x:.xs') =
  if p x
    then
      dropWhile p xs'
    else
      xs

takeWhile ::
  (a -> Bool)
  -> List a
  -> List a
takeWhile _ Nil =
  Nil
takeWhile p (x:.xs) =
  if p x
    then
      x :. takeWhile p xs
    else
      Nil

zip ::
  List a
  -> List b
  -> List (a, b)
zip =
  zipWith (,)

zipWith ::
  (a -> b -> c)
  -> List a
  -> List b
  -> List c
zipWith f (a:.as) (b:.bs) =
  f a b :. zipWith f as bs
zipWith _ _  _ =
  Nil

unfoldr ::
  (a -> Optional (b, a))
  -> a
  -> List b
unfoldr f b  =
  case f b of
    Full (a, z) -> a :. unfoldr f z
    Empty -> Nil

lines ::
  Chars
  -> List Chars
lines =
  listh . P.fmap listh . P.lines . hlist

unlines ::
  List Chars
  -> Chars
unlines =
  listh . P.unlines . hlist . map hlist

words ::
  Chars
  -> List Chars
words =
  listh . P.fmap listh . P.words . hlist

unwords ::
  List Chars
  -> Chars
unwords =
  listh . P.unwords . hlist . map hlist

listOptional ::
  (a -> Optional b)
  -> List a
  -> List b
listOptional _ Nil =
  Nil
listOptional f (h:.t) =
  let r = listOptional f t
  in case f h of
       Empty -> r
       Full q -> q :. r

any ::
  (a -> Bool)
  -> List a
  -> Bool
any p =
  foldRight ((||) . p) False

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight ((&&) . p) True

or ::
  List Bool
  -> Bool
or =
  any id

and ::
  List Bool
  -> Bool
and =
  all id

elem ::
  Eq a =>
  a
  -> List a
  -> Bool
elem x =
  any (== x)

notElem ::
  Eq a =>
  a
  -> List a
  -> Bool
notElem x =
  all (/= x)

permutations
  :: List a -> List (List a)
permutations xs0 =
  let perms Nil _ =
        Nil
      perms (t:.ts) is =
        let interleave' _ Nil r =
              (ts, r)
            interleave' f (y:.ys) r =
               let (us,zs) = interleave' (f . (y:.)) ys r
               in  (y:.us, f (t:.y:.us):.zs)
        in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
  in xs0 :. perms xs0 Nil

intersectBy ::
  (a -> b -> Bool)
  -> List a
  -> List b
  -> List a
intersectBy e xs ys =
  filter (\x -> any (e x) ys) xs

take ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
take n _  | n <= 0 =
  Nil
take _ Nil =
  Nil
take n (x:.xs) =
  x :. take (n - 1) xs

drop ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
drop n xs | n <= 0 =
  xs
drop _ Nil =
  Nil
drop n (_:.xs) =
  drop (n-1) xs

repeat ::
  a
  -> List a
repeat x =
  x :. repeat x

replicate ::
  (Num n, Ord n) =>
  n
  -> a
  -> List a
replicate n x =
  take n (repeat x)

reads ::
  P.Read a =>
  Chars
  -> Optional (a, Chars)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Chars
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Chars
  -> Optional (a, Chars)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Chars
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Chars
  -> Optional (a, Chars)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Chars
  -> Optional a
readFloat =
  mapOptional fst . readFloats

instance IsString (List Char) where
  fromString =
    listh

type Chars =
  List Char

type FilePath =
  List Char

strconcat ::
  [Chars]
  -> P.String
strconcat =
  P.concatMap hlist

stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

show' ::
  Show a =>
  a
  -> List Char
show' =
  listh . show

instance P.Functor List where
  fmap f =
    listh . P.fmap f . hlist

instance A.Applicative List where
  (<*>) =
    M.ap
  pure =
    (:. Nil)

instance P.Monad List where
  (>>=) =
    flip flatMap
  return =
    (:. Nil)
