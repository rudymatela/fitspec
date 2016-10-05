-- | Experimental module for deriving 'Mutable' and 'ShowMutable' instances
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
import Control.Monad (when, unless, liftM, liftM2)

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

-- | Derives a Mutable instance for a given type 'Name'
--   using a given context for all type variables.
deriveMutableE :: [Name] -> Name -> DecsQ
deriveMutableE = deriveMutableEX False

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
      liftM2 (++) (deriveListableIfNeeded t)
                  (if cascade
                     then reallyDeriveMutableCascade cs t
                     else reallyDeriveMutable cs t)
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

reallyDeriveMutableCascade :: [Name] -> Name -> DecsQ
reallyDeriveMutableCascade cs t = undefined

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
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD    _ _ ks _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _) -> ks
    TyConI (TySynD   _   ks   _) -> ks
#else
    TyConI (DataD    _ _ ks _ _ _) -> ks
    TyConI (NewtypeD _ _ ks _ _ _) -> ks
    TyConI (TySynD   _   ks     _) -> ks
#endif
    _                            -> error $ "error (arity): symbol "
                                         ++ show t
                                         ++ " is not a newtype, data or type synonym"

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
