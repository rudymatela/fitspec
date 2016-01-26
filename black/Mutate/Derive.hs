{-# LANGUAGE TemplateHaskell, CPP #-}
-- Experimental module for deriving Mutable instances
--
-- Needs GHC and Template Haskell (tested on GHC 7.10)
module Mutate.Derive
  ( deriveMutable
--, deriveShowMutable
  , deriveListable
--, deriveInstances
  , module Mutate
  , module Mutate.Show
  , module Test.Check
  )
where

import Language.Haskell.TH
import Test.Check
import Test.Check.Derive
import Mutate
import Mutate.Show
import Control.Monad (when, liftM2)

#if __GLASGOW_HASKELL__ < 706
-- reportWarning was only introduced in GHC 7.6 / TH 2.8
reportWarning :: String -> Q ()
reportWarning = report False
#endif

-- | Derives a Mutable instance for a given type ('Name').
deriveMutable :: Name -> DecsQ
deriveMutable t = do
  is <- t `isInstanceOf` ''Mutable
  if is
    then do reportWarning $ "Instance Mutable "
                         ++ show t
                         ++ " already exists, skipping derivation"
            return []
    else do cd <- canDeriveMutable t
            when (not cd) (fail $ "Unable to derive Mutable "
                               ++ show t)
            reallyDeriveMutable t

-- | Checks whether it is possible to derive a Mutable instance.
canDeriveMutable :: Name -> Q Bool
canDeriveMutable = (`isInstanceOf` ''Eq)

#if __GLASGOW_HASKELL__ >= 708
reallyDeriveMutable :: Name -> DecsQ
reallyDeriveMutable t = do
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  let cxt = sequence $ [[t| Eq       $(return v) |] | v <- vs]
                    ++ [[t| Listable $(return v) |] | v <- vs]
                    ++ [[t| Show     $(return v) |] | v <- vs]
#else
  let cxt = sequence $ [classP ''Eq       [return v] | v <- vs]
                    ++ [classP ''Listable [return v] | v <- vs]
                    ++ [classP ''Show     [return v] | v <- vs]
#endif
  [d| instance Mutable $(return nt)
        where lsMutants = lsMutantsEq
      instance ShowMutable $(return nt)
        where mutantS = mutantSEq |]
    `appendInstancesCxtQ` cxt
#else
reallyDeriveMutable :: Name -> DecsQ
reallyDeriveMutable t = do
  (nt,vs) <- normalizeType t
  cxt <- sequence $ [classP ''Listable [return v] | v <- vs]
                 ++ [classP ''Eq       [return v] | v <- vs]
                 ++ [classP ''Show     [return v] | v <- vs]
  return [ InstanceD
             cxt
             (AppT (ConT ''Mutable) nt)
             [ValD (VarP 'lsMutants) (NormalB (VarE 'lsMutantsEq)) []]
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
    newNames ss = mapM newName ss
    newVarTs :: Int -> Q [Type]
    newVarTs n = newNames (take n . map (:[]) . cycle $ ['a'..'z'])
             >>= return . map VarT

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
-- > [t| instance Eq a => TyCl (Ty a) where foo = goo |]
-- >   `appendInstancesCxtQ` sequence [[| Eq b |], [| Eq c |]]
-- > == [t| instance (Eq a, Eq b, Eq c) => TyCl (Ty a) where foo = goo |]
appendInstancesCxtQ :: DecsQ -> Q Cxt -> DecsQ
appendInstancesCxtQ = liftM2 $ \ds c -> map (`ac` c) ds
  where ac (InstanceD c ts ds) c' = InstanceD (c++c') ts ds
        ac d                   _  = d
