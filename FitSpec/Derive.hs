-- | Experimental module for deriving Mutable instances
--
-- Needs GHC and Template Haskell (tested on GHC 7.4, 7.6, 7.8 and 7.10)
--
-- Despite Mutable instances being actually very simple, this module can be
-- used to derive those.  However, it will not work on all cases: when that
-- happens, you should write your instances manually.
{-# LANGUAGE TemplateHaskell, CPP #-}
module FitSpec.Derive 
  ( deriveMutable
  , deriveMutableE
  , module FitSpec.Mutable
  , module FitSpec.ShowMutable
  , module Test.Check
  )
where

import FitSpec.Mutable
import FitSpec.ShowMutable

import Test.Check
import Language.Haskell.TH
import Control.Monad (when, unless, liftM, liftM2)

#if __GLASGOW_HASKELL__ < 706
-- reportWarning was only introduced in GHC 7.6 / TH 2.8
reportWarning :: String -> Q ()
reportWarning = report False
#endif

deriveListableIfNeeded :: Name -> DecsQ
deriveListableIfNeeded t = do
  is <- t `isInstanceOf` ''Listable
  if is
    then return []
    else deriveListable t

-- | Derives a Mutable instance for a given type ('Name').
deriveMutable :: Name -> DecsQ
deriveMutable = deriveMutableE []

-- | Derives a Mutable instance for a given type ('Name') using a given context
--   ('[Name]') for all type variables.
deriveMutableE :: [Name] -> Name -> DecsQ
deriveMutableE cs t = do
  is <- t `isInstanceOf` ''Mutable
  if is
    then do
      reportWarning $ "Instance Mutable " ++ show t
                   ++ " already exists, skipping derivation"
      return []
    else do
      cd <- canDeriveMutable t
      unless cd (fail $ "Unable to derive Mutable " ++ show t)
      liftM2 (++) (deriveListableIfNeeded t) (reallyDeriveMutable cs t)

-- | Checks whether it is possible to derive a Mutable instance.
canDeriveMutable :: Name -> Q Bool
canDeriveMutable t = (t `isInstanceOf` ''Eq)
                 &&& (t `isInstanceOf` ''Show)
  where (&&&) = liftM2 (&&)

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
                 where tMutants = tMutantsEq
               instance ShowMutable $(return nt)
                 where mutantS = mutantSEq |]
#else
  return [ InstanceD
             cxt
             (AppT (ConT ''Mutable) nt)
             [ValD (VarP 'tMutants) (NormalB (VarE 'tMutantsEq)) []]
         , InstanceD
             cxt
             (AppT (ConT ''ShowMutable) nt)
             [ValD (VarP 'mutantS) (NormalB (VarE 'mutantSEq)) []]
         ]
#endif


-- * Template haskell utilities

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
    TyConI (DataD    _ _ ks _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _) -> ks
    _                            -> error $ "error (arity): symbol "
                                         ++ show t
                                         ++ " is not a newtype or data"

-- Append to instance contexts in a declaration.
--
-- > sequence [[|Eq b|],[|Eq c|]] |=>| [t|instance Eq a => Cl (Ty a) where f=g|]
-- > == [t| instance (Eq a, Eq b, Eq c) => Cl (Ty a) where f = g |]
(|=>|) :: Cxt -> DecsQ -> DecsQ
c |=>| qds = do ds <- qds
                return $ map (`ac` c) ds
  where ac (InstanceD c ts ds) c' = InstanceD (c++c') ts ds
        ac d                   _  = d
