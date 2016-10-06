{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
import Test.FitSpec
import Language.Haskell.Parser
import Language.Haskell.Pretty as P
import Language.Haskell.Syntax
import Data.Ratio
import Data.List (intercalate)
import Data.Function (on)
import Data.Char

deriving instance Eq HsModule -- needed for Mutable

deriveMutableCascading ''HsModule

-- change ``take 5'' below to ``take n'' where n `elem` [1,2,3,4]
-- to see surviving mutants for different refinements
--
-- All 5 properties should be reported as ``apparently complete''
-- so no surviving mutants.
properties :: (HsModule -> String) -> [Property]
properties prettyPrint = take 5
  [ property $
      \nm loc -> (prettyPrint $ HsModule loc (Module nm) Nothing [] [])
              == "module " ++ nm ++ " where"

  , property $
      \nm loc -> (prettyPrint $ HsModule loc (Module nm) (Just []) [] [])
              == "module " ++ nm ++ " () where"

  , property $
      \nm loc -> (prettyPrint $ HsModule loc (Module nm) Nothing [] [HsFunBind []])
              == "module " ++ nm ++ " where"

  , property $
      \nm loc imports decls ->
           (prettyPrint $ HsModule loc (Module nm) Nothing imports decls)
       === unlines (("module " ++ nm ++ " where")
                   :(map P.prettyPrint imports
                  ++ map P.prettyPrint decls))

  , property $
      \nm loc imports exports decls ->
           (prettyPrint $ HsModule loc (Module nm) (Just exports) imports decls)
       === unlines (["module " ++ nm ++ " ("
                               ++ intercalate ", " (map P.prettyPrint exports)
                               ++ ") where"]
                 ++ map P.prettyPrint imports
                 ++ map P.prettyPrint decls)
  ]
  where
  (===) :: String -> String -> Bool
  (===) = (==) `on` (filter (not . null) . lines)

main = mainWith args { names = ["prettyPrint"]
                     , timeout = 0
                     }
                prettyPrint
                properties
