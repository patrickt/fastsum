{-# LANGUAGE CPP, DataKinds, TemplateHaskell, QuasiQuotes, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Sum.Templates
( mkElemIndexTypeFamily
, mkApplyInstance
) where

import Control.Monad
import Data.Traversable
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits

{- This generates a type family of the form

type family ElemIndex (t :: GHC.Types.Type
                            -> GHC.Types.Type) (ts :: [GHC.Types.Type
                                                       -> GHC.Types.Type]) :: Nat where
  ElemIndex t0 ('(:) t0 _) = 0
  ElemIndex t1 ('(:) t0 ('(:) t1 _)) = 1
  ElemIndex t2 ('(:) t0 ('(:) t1 ('(:) t2 _))) = 2
  ElemIndex t3 ('(:) t0 ('(:) t1 ('(:) t2 ('(:) t3 _)))) = 3
  ElemIndex t4 ('(:) t0 ('(:) t1 ('(:) t2 ('(:) t3 ('(:) t4 _))))) = 4
  etc...
  ElemIndex t ts = TypeError ('(:$$:) ('(:<>:) ('(:<>:) ('Text "'") ('ShowType t)) ('Text "' is not a member of the type-level list")) ('ShowType ts))

-}
mkElemIndexTypeFamily :: Integer -> DecsQ
mkElemIndexTypeFamily paramN = do
  -- Start by declaring some names.
  let [elemIndex, t, ts] = mkName <$> ["ElemIndex", "t", "ts"]
      -- Helper for building more readable type names rather than verbose gensyms
      mkT = pure . VarT . mkName . ('t' :) . show
      -- We want to make the kind signatures explicit here.
      binders = [kindedTV t  <$> [t| * -> *   |] , kindedTV ts <$> [t| [* -> *] |] ]
      -- This family ends up returning a Nat.
      resultKind = kindSig <$> [t| Nat |]
      -- We have to build n ElemIndex entries.
      equations = fmap buildEquation [0..pred paramN] ++ [errorCase]
      errorBody = [t|
        TypeError ('Text "'" ':<>: ('ShowType $(varT t)) ':<>:
                   'Text "' is not a member of the type-level list" ':$$:
                   'ShowType $(varT ts))
        |]
      -- The tySynEqn API changed in 2.15 so we need a guard here.
      -- buildEquation a single family instance equation; it uses lhsMatch
      -- to do so, making a type of the form 'ElemIndex n (n ': n0 : _)
      -- errorCase is invoked above to provide a readable error
#if MIN_VERSION_template_haskell(2,15,0)
      buildEquation n = tySynEqn Nothing (lhsMatch n) . litT . numTyLit $ n
      lhsMatch n = [t| $(conT elemIndex) $(mkT n) $(typeListT WildCardT <$> traverse mkT [0..n]) |]
      errorCase = tySynEqn Nothing [t| $(conT elemIndex) $(varT t) $(varT ts) |] errorBody
#else
      buildEquation n = tySynEqn (lhsMatch n) (litT . numTyLit $ n)
      lhsMatch n = [mkT n, typeListT WildCardT <$> traverse mkT [0..n] ]
      errorCase = tySynEqn [varT t, varT ts] errorBody
#endif

  result <- closedTypeFamilyD elemIndex
    <$> sequenceA binders
    <*> resultKind
    <*> pure Nothing
    <*> pure equations

  fmap pure result

  -- ClosedTypeFamilyD
  --   ( TypeFamilyHead
  --       elemIndex
  --       [ KindedTV t functorK
  --       , KindedTV ts (AppT ListT functorK)
  --       ]
  --       (KindSig (ConT nat))
  --       Nothing
  --   )
  --   ((mkEquation <$> [0..pred paramN]) ++ errorCase)
--   where
--         -- binders = [ kindedTV t [t| * -> * ]
--         --           ]
--         functorK = AppT (AppT ArrowT StarT) StarT
--         mkT = VarT . mkName . ('t' :) . show
--         typeErrN = mkName "TypeError"
--         textN = mkName "Text"
--         next = mkName ":<>:"
--         above = mkName ":$$:"
--         shw = mkName "ShowType"
--         -- In GHC 8.8, TySqnEqn got an extra parameter.
-- #if MIN_VERSION_template_haskell(2,15,0)
--         lhsMatch i = AppT (AppT (ConT elemIndex) (mkT i)) (typeListT WildCardT (mkT <$> [0..i]))
--         mkEquation i = TySynEqn Nothing (lhsMatch i) (LitT (NumTyLit i))
--         errorCase = [ TySynEqn
--                       Nothing
--                       (AppT (AppT (ConT elemIndex) (VarT t)) (VarT ts))
--                         (AppT
--                          (ConT typeErrN)
--                          (AppT
--                           (AppT (PromotedT above)
--                            (AppT (AppT (PromotedT next)
--                                   (AppT (AppT
--                                          (PromotedT next)
--                                          (AppT (PromotedT textN) (LitT (StrTyLit "'"))))
--                                                (AppT (PromotedT shw) (VarT t))))
--                            (AppT (PromotedT textN) (LitT (StrTyLit "' is not a member of the type-level list")))))
--                           (AppT (PromotedT shw) (VarT ts))))
--                     ]
-- #else
--         mkEquation i = TySynEqn [ mkT i, typeListT WildCardT (mkT <$> [0..i]) ] (LitT (NumTyLit i))
--         errorCase = [ TySynEqn
--                       [ VarT t , VarT ts ]
--                         (AppT
--                          (ConT typeErrN)
--                          (AppT
--                           (AppT (PromotedT above)
--                            (AppT (AppT (PromotedT next)
--                                   (AppT (AppT
--                                          (PromotedT next)
--                                          (AppT (PromotedT textN) (LitT (StrTyLit "'"))))
--                                                (AppT (PromotedT shw) (VarT t))))
--                            (AppT (PromotedT textN) (LitT (StrTyLit "' is not a member of the type-level list")))))
--                           (AppT (PromotedT shw) (VarT ts))))
--                     ]
-- #endif



mkApplyInstance :: Integer -> Dec
mkApplyInstance paramN =
  InstanceD Nothing (AppT constraint <$> typeParams) (AppT (AppT (ConT applyC) constraint) (typeListT PromotedNilT typeParams))
    [ FunD apply (zipWith mkClause [0..] typeParams)
    , PragmaD (InlineP apply Inlinable FunLike AllPhases)
    ]
  where typeParams = VarT . mkName . ('f' :) . show <$> [0..pred paramN]
        [applyC, apply, f, r, union] = mkName <$> ["Apply", "apply", "f", "r", "Sum"]
        [constraint, a] = VarT . mkName <$> ["constraint", "a"]
        mkClause i nthType = Clause
          [ VarP f, ConP union [ LitP (IntegerL i), VarP r ] ]
          (NormalB (AppE (VarE f) (SigE (AppE (VarE 'unsafeCoerce) (VarE r)) (AppT nthType a))))
          []

typeListT :: Type -> [Type] -> Type
typeListT = foldr (AppT . AppT PromotedConsT)
