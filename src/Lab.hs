--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Type classes                                                          --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Semigroup(..), Monoid(..) )

--------------------------------------------------------------------------------
-- Semigroups

-- Semigroup law:
--
-- (Associativity)      x <> (y <> z) = (x <> y) <> z

class Semigroup a where 
    (<>) :: a -> a -> a 

instance Semigroup Int where 
    (<>) x y = x + y

instance Semigroup [a] where 
    (<>) x y = x ++ y
--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mempty <> x = x 
-- (Right identity)     x <> mempty = x

class Semigroup a => Monoid a where
    mempty :: a

instance Monoid Int where
    mempty = 0

instance Monoid [a] where
    mempty = []

--------------------------------------------------------------------------------

instance Semigroup b => Semigroup (a -> b) where 
    (<>) = undefined 

instance Monoid b => Monoid (a -> b) where
    mempty  = undefined

--------------------------------------------------------------------------------
