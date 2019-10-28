{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Sum.Templates
( mkElemIndexTypeFamily
, mkApplyInstance
) where

import Language.Haskell.TH
import Unsafe.Coerce (unsafeCoerce)

mkElemIndexTypeFamily :: Integer -> DecsQ
mkElemIndexTypeFamily paramN = pure . pure $
  ClosedTypeFamilyD
    ( TypeFamilyHead
        elemIndex
        [ KindedTV t functorK
        , KindedTV ts (AppT ListT functorK)
        ]
        (KindSig (ConT nat))
        Nothing
    )
    ((mkEquation <$> [0..pred paramN]) ++ errorCase)
  where [elemIndex, t, ts, nat] = mkName <$> ["ElemIndex", "t", "ts", "Nat"]
        functorK = AppT (AppT ArrowT StarT) StarT
        mkT = VarT . mkName . ('t' :) . show
        typeErrN = mkName "TypeError"
        textN = mkName "Text"
        next = mkName ":<>:"
        above = mkName ":$$:"
        shw = mkName "ShowType"
        -- In GHC 8.8, TySqnEqn got an extra parameter.
#if MIN_VERSION_template_haskell(2,15,0)
        lhsMatch i = AppT (AppT (ConT elemIndex) (mkT i)) (typeListT WildCardT (mkT <$> [0..i]))
        mkEquation i = TySynEqn Nothing (lhsMatch i) (LitT (NumTyLit i))
        errorCase = [ TySynEqn
                      Nothing
                      (AppT (AppT (ConT elemIndex) (VarT t)) (VarT ts))
                        (AppT
                         (ConT typeErrN)
                         (AppT
                          (AppT (PromotedT above)
                           (AppT (AppT (PromotedT next)
                                  (AppT (AppT
                                         (PromotedT next)
                                         (AppT (PromotedT textN) (LitT (StrTyLit "'"))))
                                               (AppT (PromotedT shw) (VarT t))))
                           (AppT (PromotedT textN) (LitT (StrTyLit "' is not a member of the type-level list")))))
                          (AppT (PromotedT shw) (VarT ts))))
                    ]
#else
        mkEquation i = TySynEqn [ mkT i, typeListT WildCardT (mkT <$> [0..i]) ] (LitT (NumTyLit i))
        errorCase = [ TySynEqn
                      [ VarT t , VarT ts ]
                        (AppT
                         (ConT typeErrN)
                         (AppT
                          (AppT (PromotedT above)
                           (AppT (AppT (PromotedT next)
                                  (AppT (AppT
                                         (PromotedT next)
                                         (AppT (PromotedT textN) (LitT (StrTyLit "'"))))
                                               (AppT (PromotedT shw) (VarT t))))
                           (AppT (PromotedT textN) (LitT (StrTyLit "' is not a member of the type-level list")))))
                          (AppT (PromotedT shw) (VarT ts))))
                    ]
#endif



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
