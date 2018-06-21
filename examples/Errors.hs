{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts #-}
-- | A workable example for combining multiple error types into one,
-- then safely and exhaustively matching against them in an error
-- handle.
module Errors where

import Control.Applicative
import Data.Sum
import Data.Void



data Error1 = Error1 deriving (Show)
data Error2 = Error2 deriving (Show)


type family Map (f :: * -> * -> *) (xs :: [*]) where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type Errors es = Sum (Map Const es) Void


injectError :: (Const e :< r) => e -> Sum r v
injectError = inject . Const


f :: Int -> Either (Errors '[Error1, Error2]) Int
f 1 = Left (injectError Error1)
f 2 = Left (injectError Error2)
f n = Right n


e2 :: (e1 -> a) -> (e2 -> a) -> Errors '[e1, e2] -> a
e2 f1 f2 = either (f2 . getConst . decomposeLast) (f1 . getConst) . decompose

e3 :: (e1 -> a) -> (e2 -> a) -> (e3 -> a) -> Errors '[e1, e2, e3] -> a
e3 f1 f2 f3 = either (e2 f2 f3) (f1 . getConst) . decompose

-- and so on

handleErrors :: Int -> IO ()
handleErrors n = case f n of
  Left e -> putStrLn $
    e2
      (\Error1 -> "Caught Error 1")
      (\Error2 -> "Caught Error2")
      e
  Right n -> putStrLn ("Success: " ++ show n)
