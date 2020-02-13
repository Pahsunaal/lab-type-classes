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
    (<>) = (+) 

-- instance Semigroup Int where 
--     (<>) = (*) 

-- instance Semigroup Int where 
--     (<>) = max

-- instance Semigroup Int where 
--     (<>) = min

instance Semigroup [a] where 
    (<>) = (++) 

--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mempty <> x = x
-- (Right identity)     x <> mempty = x
-- (mconcat)            mconcat = foldr (<>) mempty

class Semigroup a => Monoid a where
    mempty  :: a
    mconcat :: [a] -> a
    mconcat = foldr (<>) mempty

instance Monoid Int where
    mempty  = 0

-- instance Monoid Int where
--     mempty  = 1

-- instance Monoid Int where
--     mempty  = minBound

-- instance Monoid Int where
--     mempty  = maxBound

instance Monoid [a] where
    mempty  = []

--------------------------------------------------------------------------------

instance Semigroup b => Semigroup (a -> b) where 
    -- (<>) :: Semigroup b => (a -> b) -> (a -> b) -> a -> b
    (<>) f g x = f x <> g x

instance Monoid b => Monoid (a -> b) where
    -- mempty :: Monoid b => a -> b
    mempty _ = mempty

--------------------------------------------------------------------------------

-- The following code illustrates how to work around cases where 
-- there are multiple possible instances of a particular type class
-- for a given type. 

data Plus = MkPlus Int
data Mult = MkMult Int

instance Semigroup Plus where 
    MkPlus a <> MkPlus b = MkPlus (a + b)

instance Semigroup Mult where 
    MkMult a <> MkMult b = MkMult (a * b)

--------------------------------------------------------------------------------
