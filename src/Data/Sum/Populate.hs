{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | John Wiegley is the original author of this functionality.
module Data.Sum.Populate where

import Control.Applicative (Alternative, (<|>))
import Data.Kind (Type)
import Data.Sum (Sum, inject, weaken)

class Producible m f where
  produce :: m (f v)

class Populate m (fs :: [Type -> Type]) where
  populate :: m (Sum fs b)

instance forall m t. (Producible m t, Alternative m) => Populate m '[t] where
  populate = fmap inject (produce @m @t)

instance
  {-# OVERLAPPING #-}
  (Populate m (u ': r), Producible m t, Alternative m) =>
  Populate m (t ': u ': r)
  where
  populate =
    fmap inject (produce @m @t)
      <|> fmap weaken (populate @m @(u ': r))
