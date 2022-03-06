{- | This module allows enforcing consideration of all datatype constructors at compile
time.

Sometimes you need to implement a function which produces values corresponding to all
constructors of a datatype (e.g. arbitrary transactions generator, documentation).
This module provides primitives for enlisting all constructors, which you can later
pattern match on and thus assuredly taking into account all constructors of the datatype.

-}
module Dscp.Util.Constructors
       ( CanEnlistConstructors
       , enlistConstructors
       , enlistConstructorsOf

       , UnsafeFiller
       , EnumLikeOnly
       ) where

import Universum
import Data.Default (Default (..))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Dscp.Util.Test

-- | Indicates filling constructor fields with 'error's.
class UnsafeFiller a

-- | Indicates that only datatypes with nullary constructors are allowed.
class EnumLikeOnly a

-- | Whether this filler can be applied to such field type.
type family AllowedFiller (m :: G.Meta) (fill :: * -> Constraint) :: Constraint

type instance AllowedFiller m Default = ()
type instance AllowedFiller m Arbitrary = ()

type instance AllowedFiller ('G.MetaSel f p s 'G.DecidedLazy) UnsafeFiller = ()
type instance AllowedFiller ('G.MetaSel f p s 'G.DecidedStrict) UnsafeFiller =
     TypeError ('Text "Cannot fill one of constructors in unsafe way since \
                      \it contains strict fields")
type instance AllowedFiller ('G.MetaSel f p s 'G.DecidedUnpack) UnsafeFiller =
     TypeError ('Text "Cannot fill one of constructors in unsafe way since \
                      \it contains unpacked fields")


-- | Fills fields of a single constructor.
class GFillConstructor (fill :: * -> Constraint) (a :: * -> *) where
    gFillConstructor :: HasCallStack => a p

instance GFillConstructor UnsafeFiller (G.Rec0 a) where
    gFillConstructor = error "Beyond this field only a black hole, do not touch"

instance TypeError ('Text "Cannot fill datatype with non-nullary constructors") =>
         GFillConstructor EnumLikeOnly (G.Rec0 a) where
    gFillConstructor = error "impossible"

instance Default a => GFillConstructor Default (G.Rec0 a) where
    gFillConstructor = G.K1 def

instance Arbitrary a => GFillConstructor Arbitrary (G.Rec0 a) where
    gFillConstructor = G.K1 $ detGen 0 arbitrary

instance GFillConstructor filler G.U1 where
    gFillConstructor = G.U1

instance (GFillConstructor fill x, AllowedFiller m fill) =>
         GFillConstructor fill (G.S1 m x) where
    gFillConstructor = G.M1 $ gFillConstructor @fill @x

instance (GFillConstructor fill x, GFillConstructor fill y) =>
         GFillConstructor fill (x G.:*: y) where
    gFillConstructor = gFillConstructor @fill G.:*: gFillConstructor @fill

-- | Returns list of objects built with all available constructors.
class GEnlistConstructors (fill :: * -> Constraint) (x :: * -> *) where
    -- | See it like @DList@.
    gEnlistConstructors :: HasCallStack => [x p]

instance GEnlistConstructors filler G.V1 where
    gEnlistConstructors = []

instance GEnlistConstructors fill x => GEnlistConstructors fill (G.D1 m x) where
    gEnlistConstructors = map G.M1 $ gEnlistConstructors @fill @x

instance GFillConstructor fill x => GEnlistConstructors fill (G.C1 m x) where
    gEnlistConstructors = [G.M1 (gFillConstructor @fill @x)]

instance (GEnlistConstructors fill x, GEnlistConstructors fill y) =>
         GEnlistConstructors fill (x G.:+: y) where
    gEnlistConstructors = mconcat
        [ G.L1 <$> gEnlistConstructors @fill @x
        , G.R1 <$> gEnlistConstructors @fill @y
        ]

-- | Total constraint of 'enlistConstructors'.
type CanEnlistConstructors fill a =
    (G.Generic a, GEnlistConstructors fill (G.Rep a))

{- | For each constructor generate an object via it.

You need to supply a @fill@ type argument which have to be one of:

* 'UnsafeFiller' - initialize fields as 'error's.
  Applying to a datatype with strict fields will cause a compile error.
* 'EnumLikeOnly' - just does not compile for constructors with fields.
* 'Default' - initialize fields with default values.
* 'Arbitrary' - initialize fields with arbitrarily generated values.

-}
enlistConstructors :: forall fill a. HasCallStack => CanEnlistConstructors fill a => [a]
enlistConstructors = G.to <$> gEnlistConstructors @fill

enlistConstructorsOf :: forall fill a. HasCallStack => CanEnlistConstructors fill a => a -> [a]
enlistConstructorsOf _ = enlistConstructors @fill
