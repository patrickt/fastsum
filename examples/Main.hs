{-# LANGUAGE DataKinds, DeriveFunctor, FlexibleContexts, KindSignatures, RankNTypes, TypeApplications, TypeOperators, UndecidableInstances #-}

module Main where

import Data.Monoid hiding (Sum(..))
import Data.Sum

-- okay, let's use Data.Sum to solve the expression problem
-- we'll build a little expression language, define an F-algebra, and print it out
-- you don't _have_ to use recursion schemes with Data.Sum, but they sure are nice

-- standard fixed point of a Functor. in the real world we would use an actual
-- recursion schemes library, but who has time for that?
newtype Fix f = In { out :: f (Fix f) }

-- here's our expression type - note that l is a type-level list of functors
type Expr (l :: [* -> *]) = Fix (Sum l)

-- numbers
newtype Lit a = Lit Int deriving Functor

-- smart constructor. the :< is pronounced "member":
-- what this says is that as long as Lit is a member of the type-level
-- list 'fs', we can inj it into an Expr that contains 'fs'
-- if we tried to inj it into an 'Expr [Thing1, Thing2]',
-- we would get an error message that Lit cannot be found in [Thing1, Thing2]
lit :: (Lit :< fs) => Int -> Expr fs
lit = In . inj . Lit

-- parens
newtype Paren a = Paren a
  deriving Functor

paren :: (Paren :< fs) => Expr fs -> Expr fs
paren = In . inj . Paren

-- math
data Op a
  = Add a a
  | Sub a a
  | Mul a a
    deriving Functor

(+:), (-:), (*:) :: (Op :< fs) => Expr fs -> Expr fs -> Expr fs
a +: b = In (inj (Add a b))
a -: b = In (inj (Sub a b))
a *: b = In (inj (Mul a b))

infixl 6 +:
infixl 6 -:
infixl 7 *:

-- here's our F-algebra that converts a sum type to a string
class Functor f => Pretty f where
  pretty :: f String -> String

instance Pretty Lit where
  pretty (Lit i) = show i

instance Pretty Paren where
  pretty (Paren a) = "(" <> a <> ")"

instance Pretty Op where
  pretty (Add a b) = concat [a, " + ", b]
  pretty (Sub a b) = concat [a, " - ", b]
  pretty (Mul a b) = concat [a, " * ", b]

-- this tells the compiler that any Sum type whose components
-- all implement Functor and Pretty supports pretty-printing too
instance (Apply Functor fs, Apply Pretty fs) => Pretty (Sum fs) where
  pretty = apply @Pretty pretty

-- a neutered catamorphism
runPretty :: Pretty f => Fix f -> String
runPretty = pretty . fmap runPretty . out

example :: Expr '[Lit, Op, Paren]
example = paren (lit 5 +: lit 10) *: lit 2

main :: IO ()
main = putStrLn (runPretty example)

-- now, if you so desired, you could add a new data type:
-- > data Div a = Div a a
-- declare a Pretty instance for it, and then create a new value of type
-- > Expr '[Lit, Op, Paren, Div]
-- and you get a perfect solution to the expression problem: seamless extension of functionality and of data-types
