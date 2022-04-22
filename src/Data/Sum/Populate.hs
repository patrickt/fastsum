{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides 'Populate', which allows you to construct a 'Sum' type out of
-- many individual actions such as parsers. You can think of it as the inverse of 'Apply':
-- whereas 'Apply' is an eliminator for 'Sum', 'Populate' is a constructor, and both use
-- typeclass instances to resolve behavior. For example, you could use Megaparsec's @Parser@
-- type:
-- @
--   data A x = A
--   data B x = B
--
--   instance Producible Parser A where produce = parseA
--   instance Producible Parser B where produce = parseB
--
--   parseAlt :: Parser (Sum '[A, B] x)
--   parseAlt = populate
-- @
--
-- John Wiegley is the original author of this functionality.
module Data.Sum.Populate where

import Data.Kind (Type)
import Data.Sum (Sum, inject, weaken)
import Control.Applicative

-- | An instance of 'Producible' describes how to construct a type within
-- some 'Alternative' functor like a parser.
class Producible m f where
  produce :: m (f v)

-- | 'Populate' serves as a dispatcher for instances of 'Producible'.
class Populate m (fs :: [Type -> Type]) where
  populate :: m (Sum fs b)

instance forall m t. (Producible m t, Alternative m) => Populate m '[t] where
  populate = fmap inject (produce @m @t)

-- We may need to unroll this instance in the future if people start using
-- this on absolutely massive types.
instance
  {-# OVERLAPPING #-}
  (Populate m (u ': r), Producible m t, Alternative m) =>
  Populate m (t ': u ': r)
  where
  populate =
    fmap inject (produce @m @t)
      <|> fmap weaken (populate @m @(u ': r))
