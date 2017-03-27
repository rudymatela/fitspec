-- |
-- Module      : Test.FitSpec.Derive
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Experimental module for deriving 'Mutable' and 'ShowMutable' instances
--
-- Needs GHC and Template Haskell
-- (tested on GHC 7.4, 7.6, 7.8, 7.10 and 8.0)
--
-- Despite 'Mutable' instances being actually very simple to write manually,
-- this module can be used to derive those instances automatically.
-- However, it will not work on all cases:
-- when that happens, you should write your instances manually.
--
-- If FitSpec does not compile under later GHCs, this module is probably the culprit.
{-# LANGUAGE TemplateHaskell, CPP #-}
module Test.FitSpec.Derive
  ( deriveMutable
  , deriveMutableE
  , deriveMutableCascading
  , deriveMutableCascadingE
  , module Test.FitSpec.Mutable
  , module Test.FitSpec.ShowMutable
  , module Test.LeanCheck
  )
where

import Test.FitSpec.Mutable
import Test.FitSpec.ShowMutable

import Test.LeanCheck
import Test.LeanCheck.Derive (deriveListableIfNeeded)
import Language.Haskell.TH
import Control.Monad (when, unless, liftM, liftM2, filterM)
import Data.List (delete)

#if __GLASGOW_HASKELL__ < 706
-- reportWarning was only introduced in GHC 7.6 / TH 2.8
reportWarning :: String -> Q ()
reportWarning = report False
#endif

-- | Derives 'Mutable', 'ShowMutable' and (optionally) 'Listable' instances
--   for a given type 'Name'.
--
-- Consider the following @Stack@ datatype:
--
-- > data Stack a = Stack a (Stack a) | Empty
--
-- Writing
--
-- > deriveMutable ''Stack
--
-- will automatically derive the following
-- 'Listable', 'Mutable' and 'ShowMutable' instances:
--
-- > instance Listable a => Listable (Stack a) where
-- >   tiers = cons2 Stack \/ cons0 Empty
-- >
-- > instance (Eq a, Listable a) => Mutable a
-- >   where mutiers = mutiersEq
-- >
-- > instance (Eq a, Show a) => ShowMutable a
-- >   where mutantS = mutantSEq
--
-- If a 'Listable' instance already exists, it is not derived.
-- (cf.: 'deriveListable')
--
-- Needs the @TemplateHaskell@ extension.
deriveMutable :: Name -> DecsQ
deriveMutable = deriveMutableE []

deriveMutableCascading :: Name -> DecsQ
deriveMutableCascading = deriveMutableCascadingE []

-- | Derives a Mutable instance for a given type 'Name'
--   using a given context for all type variables.
deriveMutableE :: [Name] -> Name -> DecsQ
deriveMutableE = deriveMutableEX False

deriveMutableCascadingE :: [Name] -> Name -> DecsQ
deriveMutableCascadingE = deriveMutableEX True

deriveMutableEX :: Bool -> [Name] -> Name -> DecsQ
deriveMutableEX cascade cs t = do
  is <- t `isInstanceOf` ''Mutable
  if is
    then do
      reportWarning $ "Instance Mutable " ++ show t
                   ++ " already exists, skipping derivation"
      return []
    else do
      isEq   <- t `isInstanceOf` ''Eq
      isShow <- t `isInstanceOf` ''Show
      unless isEq   (fail $ "Unable to derive Mutable " ++ show t
                         ++ " (missing Eq instance)")
      unless isShow (fail $ "Unable to derive Mutable " ++ show t
                         ++ " (missing Show instance)")
      if cascade
        then liftM2 (++) (deriveListableCascading t) (reallyDeriveMutableCascading cs t)
        else liftM2 (++) (deriveListableIfNeeded t) (reallyDeriveMutable cs t)
-- TODO: document deriveMutableE with an example
-- TODO: create deriveListableE on LeanCheck?

reallyDeriveMutable :: [Name] -> Name -> DecsQ
reallyDeriveMutable cs t = do
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  cxt <- sequence [ [t| $(conT c) $(return v) |]
#else
  cxt <- sequence [ classP c [return v]
#endif
                  | v <- vs, c <- ''Eq:''Listable:''Show:cs ]
#if __GLASGOW_HASKELL__ >= 708
  cxt |=>| [d| instance Mutable $(return nt)
                 where mutiers = mutiersEq
               instance ShowMutable $(return nt)
                 where mutantS = mutantSEq |]
#else
  return [ InstanceD
             cxt
             (AppT (ConT ''Mutable) nt)
             [ValD (VarP 'mutiers) (NormalB (VarE 'mutiersEq)) []]
         , InstanceD
             cxt
             (AppT (ConT ''ShowMutable) nt)
             [ValD (VarP 'mutantS) (NormalB (VarE 'mutantSEq)) []]
         ]
#endif

reallyDeriveMutableCascading :: [Name] -> Name -> DecsQ
reallyDeriveMutableCascading cs t = do
      return . concat
  =<< mapM (reallyDeriveMutable cs)
  =<< filterM (liftM not . isTypeSynonym)
  =<< return . (t:) . delete t
  =<< t `typeConCascadingArgsThat` (`isntInstanceOf` ''Mutable)


-- * Template haskell utilities

typeConArgs :: Name -> Q [Name]
typeConArgs t = do
  is <- isTypeSynonym t
  if is
    then liftM typeConTs $ typeSynonymType t
    else liftM (nubMerges . map typeConTs . concat . map snd) $ typeConstructors t
  where
  typeConTs :: Type -> [Name]
  typeConTs (AppT t1 t2) = typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (SigT t _) = typeConTs t
  typeConTs (VarT _) = []
  typeConTs (ConT n) = [n]
#if __GLASGOW_HASKELL__ >= 800
  -- typeConTs (PromotedT n) = [n] ?
  typeConTs (InfixT  t1 n t2) = typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (UInfixT t1 n t2) = typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (ParensT t) = typeConTs t
#endif
  typeConTs _ = []

typeConArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
typeConArgsThat t p = do
  targs <- typeConArgs t
  tbs   <- mapM (\t' -> do is <- p t'; return (t',is)) targs
  return [t' | (t',p) <- tbs, p]

typeConCascadingArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
t `typeConCascadingArgsThat` p = do
  ts <- t `typeConArgsThat` p
  let p' t' = do is <- p t'; return $ t' `notElem` (t:ts) && is
  tss <- mapM (`typeConCascadingArgsThat` p') ts
  return $ nubMerges (ts:tss)

-- Normalizes a type by applying it to necessary type variables, making it
-- accept "zero" parameters.  The normalized type is tupled with a list of
-- necessary type variables.
--
-- Suppose:
--
-- > data DT a b c ... = ...
--
-- Then, in pseudo-TH:
--
-- > normalizeType [t|DT|] == Q (DT a b c ..., [a, b, c, ...])
normalizeType :: Name -> Q (Type, [Type])
normalizeType t = do
  ar <- typeArity t
  vs <- newVarTs ar
  return (foldl AppT (ConT t) vs, vs)
  where
    newNames :: [String] -> Q [Name]
    newNames = mapM newName
    newVarTs :: Int -> Q [Type]
    newVarTs n = liftM (map VarT)
               $ newNames (take n . map (:[]) $ cycle ['a'..'z'])

-- Normalizes a type by applying it to units (`()`) while possible.
--
-- > normalizeTypeUnits ''Int    === [t| Int |]
-- > normalizeTypeUnits ''Maybe  === [t| Maybe () |]
-- > normalizeTypeUnits ''Either === [t| Either () () |]
normalizeTypeUnits :: Name -> Q Type
normalizeTypeUnits t = do
  ar <- typeArity t
  return (foldl AppT (ConT t) (replicate ar (TupleT 0)))

-- Given a type name and a class name,
-- returns whether the type is an instance of that class.
isInstanceOf :: Name -> Name -> Q Bool
isInstanceOf tn cl = do
  ty <- normalizeTypeUnits tn
  isInstance cl [ty]

isntInstanceOf :: Name -> Name -> Q Bool
isntInstanceOf tn cl = liftM not (isInstanceOf tn cl)

-- | Given a type name, return the number of arguments taken by that type.
-- Examples in partially broken TH:
--
-- > arity ''Int        === Q 0
-- > arity ''Int->Int   === Q 0
-- > arity ''Maybe      === Q 1
-- > arity ''Either     === Q 2
-- > arity ''Int->      === Q 1
--
-- This works for Data's and Newtype's and it is useful when generating
-- typeclass instances.
typeArity :: Name -> Q Int
typeArity t = do
  ti <- reify t
  return . length $ case ti of
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD    _ _ ks _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _) -> ks
#else
    TyConI (DataD    _ _ ks _ _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _ _) -> ks
#endif
    TyConI (TySynD _ ks _) -> ks
    _ -> error $ "error (typeArity): symbol " ++ show t
              ++ " is not a newtype, data or type synonym"

-- Given a type name, returns a list of its type constructor names paired with
-- the type arguments they take.
--
-- > typeConstructors ''()    === Q [('(),[])]
--
-- > typeConstructors ''(,)   === Q [('(,),[VarT a, VarT b])]
--
-- > typeConstructors ''[]    === Q [('[],[]),('(:),[VarT a,AppT ListT (VarT a)])]
--
-- > data Pair a = P a a
-- > typeConstructors ''Pair  === Q [('P,[VarT a, VarT a])]
--
-- > data Point = Pt Int Int
-- > typeConstructors ''Point === Q [('Pt,[ConT Int, ConT Int])]
typeConstructors :: Name -> Q [(Name,[Type])]
typeConstructors t = do
  ti <- reify t
  return . map simplify $ case ti of
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD    _ _ _ cs _) -> cs
    TyConI (NewtypeD _ _ _ c  _) -> [c]
#else
    TyConI (DataD    _ _ _ _ cs _) -> cs
    TyConI (NewtypeD _ _ _ _ c  _) -> [c]
#endif
    _ -> error $ "error (typeConstructors): symbol " ++ show t
              ++ " is neither newtype nor data"
  where
  simplify (NormalC n ts)  = (n,map snd ts)
  simplify (RecC    n ts)  = (n,map trd ts)
  simplify (InfixC  t1 n t2) = (n,[snd t1,snd t2])
  trd (x,y,z) = z

isTypeSynonym :: Name -> Q Bool
isTypeSynonym t = do
  ti <- reify t
  return $ case ti of
    TyConI (TySynD _ _ _) -> True
    _                     -> False

typeSynonymType :: Name -> Q Type
typeSynonymType t = do
  ti <- reify t
  return $ case ti of
    TyConI (TySynD _ _ t') -> t'
    _ -> error $ "error (typeSynonymType): symbol " ++ show t
              ++ " is not a type synonym"

-- Append to instance contexts in a declaration.
--
-- > sequence [[|Eq b|],[|Eq c|]] |=>| [t|instance Eq a => Cl (Ty a) where f=g|]
-- > == [t| instance (Eq a, Eq b, Eq c) => Cl (Ty a) where f = g |]
(|=>|) :: Cxt -> DecsQ -> DecsQ
c |=>| qds = do ds <- qds
                return $ map (`ac` c) ds
#if __GLASGOW_HASKELL__ < 800
  where ac (InstanceD c ts ds) c' = InstanceD (c++c') ts ds
        ac d                   _  = d
#else
  where ac (InstanceD o c ts ds) c' = InstanceD o (c++c') ts ds
        ac d                     _  = d
#endif

-- > nubMerge xs ys == nub (merge xs ys)
-- > nubMerge xs ys == nub (sort (xs ++ ys))
nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge [] ys = ys
nubMerge xs [] = xs
nubMerge (x:xs) (y:ys) | x < y     = x :    xs  `nubMerge` (y:ys)
                       | x > y     = y : (x:xs) `nubMerge`    ys
                       | otherwise = x :    xs  `nubMerge`    ys

nubMerges :: Ord a => [[a]] -> [a]
nubMerges = foldr nubMerge []
