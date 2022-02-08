{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Data.Sum
Description : Open sums (type-indexed co-products) for extensible effects.
Copyright   : Allele Dev 2015
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

All operations are constant-time, and there is no Typeable constraint.

This is a variation of Kiselyov's OpenUnion5.hs, which relies on
overlapping instances instead of closed type families. Closed type
families have their problems: overlapping instances can resolve even
for unground types, but closed type families are subject to a strict
apartness condition.
-}

module Data.Sum
  ( -- * The fundamental sum-of-products type
    Sum
  -- * Creating and extracting sums from products
  , inject
  , project
  , projectGuard
  -- * Operating on sums' effects lists
  , decompose
  , decomposeLast
  , weaken
  -- * Membership prodicates
  , Element
  , type(:<)
  , Elements
  , type(:<:)
  , ElemIndex
  , elemIndex
  -- * Typeclass application.
  , Apply(..)
  , apply'
  , apply2
  , apply2'
  ) where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData(..), NFData1(..))
import Data.Functor.Classes (Eq1(..), eq1, Ord1(..), compare1, Show1(..), showsPrec1)
import Data.Hashable (Hashable(..))
import Data.Hashable.Lifted (Hashable1(..), hashWithSalt1)
import Data.Maybe (fromMaybe)
import Data.Sum.Templates
import GHC.Exts (Constraint)
import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits
import Unsafe.Coerce(unsafeCoerce)

mkElemIndexTypeFamily 200

infixr 5 :<

-- | The fundamental sum type over a type-level list of products @r@
-- and an annotation type @v@. The constructor is not exported;
-- use 'inject' to create a 'Sum'.
data Sum (r :: [ * -> * ]) (v :: *) where
  -- | Strong Sum (Existential with the evidence) is an open sum
  -- t is can be a GADT and hence not necessarily a Functor.
  -- Int is the index of t in the list r; that is, the index of t in the
  -- universe r.
  Sum :: {-# UNPACK #-} !Int -> t v -> Sum r v

unsafeInject :: Int -> t v -> Sum r v
unsafeInject = Sum
{-# INLINE unsafeInject #-}

unsafeProject :: Int -> Sum r v -> Maybe (t v)
unsafeProject n (Sum n' x) | n == n'   = Just (unsafeCoerce x)
                           | otherwise = Nothing
{-# INLINE unsafeProject #-}

newtype P (t :: * -> *) (r :: [* -> *]) = P { unP :: Int }

infixr 5 :<:
-- | An @Elements ms r@ constraint proves that @r@ contains
-- all of the elements in @ms@.
type family Elements (ms :: [* -> *]) r :: Constraint where
  Elements (t ': cs) r = (Element t r, Elements cs r)
  Elements '[] r = ()

-- | An infix synonym for 'Elements'.
type (ts :<: r) = Elements ts r

-- | Inject a functor into a type-aligned sum.
inject :: forall e r v. (e :< r) => e v -> Sum r v
inject = unsafeInject (unP (elemNo :: P e r))
{-# INLINE inject #-}

-- | Maybe project a functor out of a type-aligned sum.
project :: forall e r v. (e :< r) => Sum r v -> Maybe (e v)
project = unsafeProject (unP (elemNo :: P e r))
{-# INLINE project #-}

-- | As 'project', but generalized to any 'Alternative' functor.
projectGuard :: forall m e r v . (Alternative m, e :< r) => Sum r v -> m (e v)
projectGuard = maybe empty pure . project

-- | Attempts to extract the head type @e@ from a @Sum@. Returns
-- @Right@ on success, and a @Sum@ without @e@ otherwise. You can
-- repeatedly apply this and apply 'decomposeLast' when you have @Sum
-- '[e]@ to get typesafe, exhaustive matching of an open sum. See
-- @examples/Errors.hs@ for a full example.
decompose :: Sum (e ': es) b -> Either (Sum es b) (e b)
decompose sum@(Sum n v) = maybe (Left (Sum (n - 1) v)) Right (project sum)
{-# INLINE decompose #-}

-- | Special case of 'decompose' which knows that there is only one
-- possible type remaining in the @Sum@, @e@ thus it is guaranteed to
-- return @e@
decomposeLast :: Sum '[e] b -> e b
decomposeLast = either (error "Data.Sum: impossible case in decomposeLast") id . decompose
{-# INLINE decomposeLast #-}

-- | Add an arbitrary product @any@ to a product list @r@.
weaken :: Sum r w -> Sum (any ': r) w
weaken (Sum n v) = Sum (n+1) v

-- | @Element t r@ is a proof that @t@ is a member of @r@. This is implemented
-- in terms of @KnownNat@ rather than recursive typeclass lookups.
type (Element t r) = KnownNat (ElemIndex t r)

-- | An infix version of 'Element'. Note that you will need @-XTypeOperators@
-- turned on to use this.
type (t :< r) = Element t r

elemIndex :: Sum r w -> Int
elemIndex (Sum n _) = n

-- Find an index of an element in an `r'.
-- The element must exist, so this is essentially a compile-time computation.
elemNo :: forall t r . (t :< r) => P t r
elemNo = P (fromIntegral (natVal' (proxy# :: Proxy# (ElemIndex t r))))

-- | Helper to apply a function to a functor of the nth type in a type list.
-- An @Apply SomeClass fs@ instance means that @Sum fs@ has an instance of @SomeClass@.
-- Instances are written using 'apply' and an explicit type application:
--
-- > instance Apply SomeClass fs => SomeClass (Sum fs) where method = apply @SomeClass method
--
-- An @INLINEABLE@ pragma on such an instance may improve dispatch speed.
class Apply (c :: (* -> *) -> Constraint) (fs :: [* -> *]) where
  apply :: (forall g . c g => g a -> b) -> Sum fs a -> b

apply' :: forall c fs a b . Apply c fs => (forall g . c g => (forall x. g x -> Sum fs x) -> g a -> b) -> Sum fs a -> b
apply' f u@(Sum n _) = apply @c (f (Sum n)) u
{-# INLINABLE apply' #-}

apply2 :: forall c fs a b d . Apply c fs => (forall g . c g => g a -> g b -> d) -> Sum fs a -> Sum fs b -> Maybe d
apply2 f u@(Sum n1 _) (Sum n2 r2)
  | n1 == n2  = Just (apply @c (\ r1 -> f r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing
{-# INLINABLE apply2 #-}

apply2' :: forall c fs a b d . Apply c fs => (forall g . c g => (forall x. g x -> Sum fs x) -> g a -> g b -> d) -> Sum fs a -> Sum fs b -> Maybe d
apply2' f u@(Sum n1 _) (Sum n2 r2)
  | n1 == n2  = Just (apply' @c (\ reinject r1 -> f reinject r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing
{-# INLINABLE apply2' #-}

pure (mkApplyInstance <$> [1..200])


instance Apply Foldable fs => Foldable (Sum fs) where
  foldMap f = apply @Foldable (foldMap f)
  {-# INLINABLE foldMap #-}

  foldr combine seed = apply @Foldable (foldr combine seed)
  {-# INLINABLE foldr #-}

  foldl combine seed = apply @Foldable (foldl combine seed)
  {-# INLINABLE foldl #-}

  null = apply @Foldable null
  {-# INLINABLE null #-}

  length = apply @Foldable length
  {-# INLINABLE length #-}

instance Apply Functor fs => Functor (Sum fs) where
  fmap f = apply' @Functor (\ reinject a -> reinject (fmap f a))
  {-# INLINABLE fmap #-}

  (<$) v = apply' @Functor (\ reinject a -> reinject (v <$ a))
  {-# INLINABLE (<$) #-}

instance (Apply Foldable fs, Apply Functor fs, Apply Traversable fs) => Traversable (Sum fs) where
  traverse f = apply' @Traversable (\ reinject a -> reinject <$> traverse f a)
  {-# INLINABLE traverse #-}

  sequenceA = apply' @Traversable (\ reinject a -> reinject <$> sequenceA a)
  {-# INLINABLE sequenceA #-}


instance Apply Eq1 fs => Eq1 (Sum fs) where
  liftEq eq u1 u2 = fromMaybe False (apply2 @Eq1 (liftEq eq) u1 u2)
  {-# INLINABLE liftEq #-}

instance (Apply Eq1 fs, Eq a) => Eq (Sum fs a) where
  (==) = eq1
  {-# INLINABLE (==) #-}


instance (Apply Eq1 fs, Apply Ord1 fs) => Ord1 (Sum fs) where
  liftCompare compareA u1@(Sum n1 _) u2@(Sum n2 _) = fromMaybe (compare n1 n2) (apply2 @Ord1 (liftCompare compareA) u1 u2)
  {-# INLINABLE liftCompare #-}

instance (Apply Eq1 fs, Apply Ord1 fs, Ord a) => Ord (Sum fs a) where
  compare = compare1
  {-# INLINABLE compare #-}


instance Apply Show1 fs => Show1 (Sum fs) where
  liftShowsPrec sp sl d = apply @Show1 (liftShowsPrec sp sl d)
  {-# INLINABLE liftShowsPrec #-}

instance (Apply Show1 fs, Show a) => Show (Sum fs a) where
  showsPrec = showsPrec1
  {-# INLINABLE showsPrec #-}


instance (Apply Eq1 fs, Apply Hashable1 fs) => Hashable1 (Sum fs) where
  liftHashWithSalt hashWithSalt' salt u@(Sum n _) = salt `hashWithSalt` apply @Hashable1 (liftHashWithSalt hashWithSalt' n) u
  {-# INLINABLE liftHashWithSalt #-}

instance (Apply Eq1 fs, Apply Hashable1 fs, Hashable a) => Hashable (Sum fs a) where
  hashWithSalt = hashWithSalt1
  {-# INLINABLE hashWithSalt #-}

instance Apply NFData1 fs => NFData1 (Sum fs) where
  liftRnf rnf' u@(Sum n _) = apply @NFData1 (liftRnf rnf') u
  {-# INLINABLE liftRnf #-}

instance (Apply NFData1 fs, NFData a) => NFData (Sum fs a) where
  rnf u@(Sum n _) = rnf n `seq` liftRnf rnf u
  {-# INLINABLE rnf #-}
