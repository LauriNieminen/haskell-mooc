-- Exercise set 5a
--
-- * defining algebraic datatypes
-- * recursive datatypes


module Set5a where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Define the type Vehicle that has four constructors: Bike,
-- Bus, Tram and Train.
--
-- The constructors don't need any fields.
data Vehicle = Bike | Bus | Tram | Train

------------------------------------------------------------------------------
-- Ex 2: Define the type BusTicket that can represent values like these:
--  - SingleTicket
--  - MonthlyTicket "January"
--  - MonthlyTicket "December"
data BusTicket = SingleTicket | MonthlyTicket String

------------------------------------------------------------------------------
-- Ex 3: Here's the definition for a datatype ShoppingEntry that
-- represents an entry in a shopping basket. It has an item name (a
-- String), an item price (a Double) and a count (an Int). You'll also
-- find two examples of ShoppingEntry values.
--
-- Implement the functions totalPrice and buyOneMore below.

data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

threeApples :: ShoppingEntry
threeApples = MkShoppingEntry "Apple" 0.5 3

twoBananas :: ShoppingEntry
twoBananas = MkShoppingEntry "Banana" 1.1 2

-- totalPrice should return the total price for an entry
--
-- Hint: you'll probably need fromIntegral to convert the Int into a
-- Double
--
-- Examples:
--   totalPrice threeApples  ==> 1.5
--   totalPrice twoBananas   ==> 2.2

totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry _ price count) = price * (fromIntegral count)

-- buyOneMore should increment the count in an entry by one
--
-- Example:
--   buyOneMore twoBananas    ==> MkShoppingEntry "Banana" 1.1 3

buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry entry price count) = MkShoppingEntry entry price (count+1) 

------------------------------------------------------------------------------
-- Ex 4: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getName,
-- setAge and setName (see below).

data Person = MkPerson Int String 
  deriving Show

-- fred is a person whose name is Fred and age is 90
fred :: Person
fred = MkPerson 90 "Fred"

-- getName returns the name of the person
getName :: Person -> String
getName (MkPerson _ name) = name

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (MkPerson age _) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName name (MkPerson age _) = MkPerson age name

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge age (MkPerson _ name) = MkPerson age name

------------------------------------------------------------------------------
-- Ex 5: define a datatype Position which contains two Int values, x
-- and y. Also define the functions below for operating on a Position.
--
-- Examples:
--   getY (up (up origin))    ==> 2
--   getX (up (right origin)) ==> 1

data Position = Position Int Int

-- origin is a Position value with x and y set to 0
origin :: Position
origin = Position 0 0

-- getX returns the x of a Position
getX :: Position -> Int
getX (Position x _) = x 

-- getY returns the y of a position
getY :: Position -> Int
getY (Position _ y) = y 

-- up increases the y value of a position by one
up :: Position -> Position
up (Position x y) = Position x (y + 1)

-- right increases the x value of a position by one
right :: Position -> Position
right (Position x y) = Position (x + 1) y

------------------------------------------------------------------------------
-- Ex 6: Here's a datatype that represents a student. A student can
-- either be a freshman, a nth year student, or graduated.

data Student = Freshman | NthYear Int | Graduated
  deriving (Show,Eq)

-- Implement the function study, which changes a Freshman into a 1st
-- year student, a 1st year student into a 2nd year student, and so
-- on. A 7th year student gets changed to a graduated student. A
-- graduated student stays graduated even if he studies.

study :: Student -> Student
study Freshman = NthYear 1
study (NthYear 7) = Graduated
study (NthYear n) = NthYear (n + 1)
study Graduated = Graduated

------------------------------------------------------------------------------
-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in increasing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype (feel free to name the
-- constructors however you want)
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

data UpDown = Up Int | Down Int

-- zero is an increasing counter with value 0
zero :: UpDown
zero = Up 0 

-- get returns the counter value
get :: UpDown -> Int
get (Up n) = n
get (Down n) = n

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
tick (Up n) = Up (n + 1)
tick (Down n) = Down (n - 1)

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
toggle (Up n) = Down n
toggle (Down n) = Up n

------------------------------------------------------------------------------
-- Ex 8: you'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Invert.
--
-- Mix means the average of the two colors in each rgb channel.
--
-- Invert means subtracting all rgb values from 1.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
--
-- Examples:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
--
-- rgb (Mix Red Green)                    ==> [0.5,0.5,0]
-- rgb (Mix Red (Mix Red Green))          ==> [0.75,0.25,0]
-- rgb (Invert Red)                       ==> [0,1,1]
-- rgb (Invert (Mix Red (Mix Red Green))) ==> [0.25,0.75,1]
-- rgb (Mix (Invert Red) (Invert Green))  ==> [0.5,0.5,1]

data Color = Red | Green | Blue | Mix Color Color | Invert Color
  deriving Show

rgb :: Color -> [Double]
rgb Red = [1.0, 0.0, 0.0]
rgb Green = [0.0, 1.0, 0.0]
rgb Blue = [0.0, 0.0 ,1.0]

rgb (Mix c1 c2) = mixer (rgb c1) (rgb c2) 
rgb (Invert c) = inverter (rgb c)

inverter :: [Double] -> [Double]
inverter xs = map (\x -> 1.0 - x) xs


mixer :: [Double] -> [Double] -> [Double]
mixer xs ys = mixer' xs ys []

mixer' :: [Double] -> [Double] -> [Double] -> [Double]
mixer' [] [] result = result
mixer' (x:xs) (y:ys) result = mixer' xs ys (result ++ [(x+y) / 2.0])


------------------------------------------------------------------------------
-- Ex 9: define a parameterized datatype OneOrTwo that contains one or
-- two values of the given type. The constructors should be called One and Two.
--
-- Examples:
--   One True         ::  OneOrTwo Bool
--   Two "cat" "dog"  ::  OneOrTwo String

data OneOrTwo a = One a | Two a a


------------------------------------------------------------------------------
-- Ex 10: define a recursive datatype KeyVals for storing a set of
-- key-value pairs. There should be two constructors: Empty and Pair.
--
-- Empty represents an empty collection. It should have no fields.
--
-- Pair should have three fields, one for the key, one for the value,
-- and one for the rest of the collection (of type KeyVals)
--
-- The KeyVals datatype is parameterized by the key type k and
-- the value type v.
--
-- For example:
--
--  Pair "cat" True (Pair "dog" False Empty)  ::  KeyVals String Bool
--
-- Also define the functions toList and fromList that convert between
-- KeyVals and lists of pairs.

data KeyVals k v = Pair k v (KeyVals k v) | Empty
  deriving Show

toList :: KeyVals k v -> [(k,v)]
toList kvs = toList' kvs []

toList' :: (KeyVals k v) -> [(k,v)] -> [(k,v)]
toList' Empty result = result
toList' (Pair k v kvs) result = toList' kvs (result ++ [(k, v)])

fromList :: [(k,v)] -> KeyVals k v
fromList kvs = fromList' (reverse kvs) Empty

fromList' :: [(k,v)] -> KeyVals k v -> KeyVals k v
fromList' [] result = result
fromList' (x:xs) result = fromList' xs (Pair (fst x) (snd x) result)

------------------------------------------------------------------------------
-- Ex 11: The data type Nat is the so called Peano
-- representation for natural numbers. Define functions fromNat and
-- toNat that convert natural numbers to Ints and vice versa.
--
-- Examples:
--   fromNat (PlusOne (PlusOne (PlusOne Zero)))  ==>  3
--   toNat 3    ==> Just (PlusOne (PlusOne (PlusOne Zero)))
--   toNat (-3) ==> Nothing
--

data Nat = Zero | PlusOne Nat
  deriving (Show,Eq)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (PlusOne n) = 1 + (fromNat n)

toNat :: Int -> Maybe Nat
toNat z
  | z < 0 = Nothing
  | otherwise = Just (toNat' z Zero)

toNat' :: Int -> Nat -> Nat
toNat' 0 result = result
toNat' z result = toNat' (z - 1) (PlusOne result)

