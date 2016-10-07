{-# LANGUAGE TemplateHaskell #-}
import Test.FitSpec
import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty

deriveMutable ''Activation
deriveMutable ''Alt
deriveMutable ''Annotation
deriveMutable ''Assoc
deriveMutable ''Asst
deriveMutable ''BangType
deriveMutable ''Binds
deriveMutable ''BooleanFormula
deriveMutable ''Boxed
deriveMutable ''Bracket
deriveMutable ''CallConv
deriveMutable ''ClassDecl
deriveMutable ''CName
deriveMutable ''ConDecl
deriveMutable ''Context
deriveMutable ''DataOrNew
deriveMutable ''Decl
deriveMutable ''DeclHead
deriveMutable ''Deriving
deriveMutable ''EWildcard
deriveMutable ''Exp
deriveMutable ''ExportSpec
deriveMutable ''ExportSpecList
deriveMutable ''FieldDecl
deriveMutable ''FieldUpdate
deriveMutable ''FunDep
deriveMutable ''GadtDecl
deriveMutable ''GuardedRhs
deriveMutable ''ImportDecl
deriveMutable ''ImportSpec
deriveMutable ''ImportSpecList
deriveMutable ''InjectivityInfo
deriveMutable ''InstDecl
deriveMutable ''InstHead
deriveMutable ''InstRule
deriveMutable ''IPBind
deriveMutable ''IPName
deriveMutable ''Kind
deriveMutable ''Literal
deriveMutable ''Match
deriveMutable ''Module
deriveMutable ''ModuleHead
deriveMutable ''ModuleName
deriveMutable ''ModulePragma
deriveMutable ''Name
deriveMutable ''Namespace
deriveMutable ''Op
deriveMutable ''Overlap
deriveMutable ''Pat
deriveMutable ''PatField
deriveMutable ''PatternSynDirection
deriveMutable ''Promoted
deriveMutable ''PXAttr
deriveMutable ''QName
deriveMutable ''QOp
deriveMutable ''QualConDecl
deriveMutable ''QualStmt
deriveMutable ''ResultSig
deriveMutable ''Rhs
deriveMutable ''Role
deriveMutable ''RPat
deriveMutable ''RPatOp
deriveMutable ''Rule
deriveMutable ''RuleVar
deriveMutable ''Safety
deriveMutable ''Sign
deriveMutable ''SpecialCon
deriveMutable ''Splice
deriveMutable ''Stmt
deriveMutable ''Tool
deriveMutable ''Type
deriveMutable ''TypeEqn
deriveMutable ''TyVarBind
deriveMutable ''Unpackedness
deriveMutable ''WarningText
deriveMutable ''XAttr
deriveMutable ''XName

instance Eq a => Eq (ParseResult a) where
  ParseOk x == ParseOk y = x == y
  ParseFailed l1 s1 == ParseFailed l2 s2 = l1 == l2 && s1 == s2
  _ == _ = False

deriveMutable ''SrcLoc
deriveMutable ''ParseResult
deriveMutable ''SrcSpanInfo
deriveMutable ''SrcSpan

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
