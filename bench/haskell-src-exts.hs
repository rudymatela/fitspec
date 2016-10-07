{-# LANGUAGE TemplateHaskell #-}
import Test.FitSpec
import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty

instance Eq a => Eq (ParseResult a) where
  ParseOk x == ParseOk y = x == y
  ParseFailed l1 s1 == ParseFailed l2 s2 = l1 == l2 && s1 == s2
  _ == _ = False

deriveMutableCascading ''ParseResult
deriveMutableCascading ''Module
deriveMutableCascading ''SrcSpanInfo

properties :: (String -> ParseResult (Module SrcSpanInfo)
              ,Module SrcSpanInfo -> String)
           -> [Property]
properties (parseFileContents,prettyPrint) =
  [ property $ case parseFileContents "" of
                 ParseOk (Module _ _ _ _ _) -> True
                 _ -> False
  , property $ case parseFileContents "a" of
                 ParseFailed (SrcLoc "<unknown>.hs" 2 1) _ -> True
                 _ -> False
  , property $ \m -> case parseFileContents (prettyPrint m) of
                       ParseOk m' -> case parseFileContents (prettyPrint m') of
                                       ParseOk m'' -> m' == m''
                                       _ -> False
                       _ -> True
-- Not true:
--, property $ \s -> case parseFileContents s of
--                     ParseOk m' -> prettyPrint m' == s
--                     _ -> True
  ]

main = mainWith args { names = ["parseFileContents f", "prettyPrint m"] }
                (parseFileContents,prettyPrint)
                properties
