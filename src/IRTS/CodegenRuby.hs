module IRTS.CodegenRuby(codegenRuby) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char

import Text.PrettyPrint hiding (Str)

rubyPreamble :: Doc
rubyPreamble = vcat . map text $
  [ "def idris_error(str); puts str; exit(0); end",
    "def idris_writeStr(str); puts str; end",
    "def idris_readStr;return gets;end",
    "def idris_append(l, r);return l + r;end",
    "def idris_is_none(l,r); l == r ? 1 : 0;end" ]

codegenRuby :: CodeGenerator
codegenRuby ci = writeFile (outputFile ci) (render source) 
  where
    source = rubyPreamble $+$ br $+$ (text out) $+$ start
    out = concatMap (show . doCodegen) (simpleDecls ci)
    start = rubyname (sMN 0 "runMain")

rubyname :: Name -> Doc
rubyname n = text "idris_" <> (text $ concatMap rubychar (showCG n))
  where rubychar x | isAlpha x || isDigit x = [x]
                   | otherwise = "_" ++ show (fromEnum x) ++ "_"

var :: Name -> Doc
var n = rubyname n

loc :: Int -> Doc
loc i = text "loc" <> (text $ show i)

doCodegen :: (Name, SDecl) -> Doc
doCodegen (n, SFun _ args i def) = cgFun n args def

cgComment :: Doc -> Doc
cgComment t = text "#" <+> t

cgIndent :: Doc
cgIndent = text "  "

doRet :: Doc -> Doc
doRet str = cgIndent <> text "return" <+> str

def :: Doc
def = text "def"

end :: Doc
end = text "end"

br :: Doc
br = text "\n"

when :: Doc
when = text "when"

cgFun :: Name -> [Name] -> SExp -> Doc
cgFun n args d
    = comment $+$ function $+$ br
    where
      function = def <+> signature $+$ body $+$ end
      signature = rubyname n
        <> lparen <> text (showSep "," (map (show . loc . fst) (zip [0..] args)))
        <> rparen
      body = cgBody doRet d
      comment = cgComment (text $ (show n))

-- cgBody converts the SExp into a chunk of php which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgBody :: (Doc -> Doc) -> SExp -> Doc
cgBody ret (SV (Glob n)) = ret $ rubyname n
cgBody ret (SV (Loc i)) = ret $ loc i 
cgBody ret (SApp _ f args) = ret $ rubyname f <> lparen <> 
                                   text (showSep "," (map (show . cgVar) args)) <> rparen
cgBody ret (SLet (Loc i) v sc)
   = cgBody (\x -> cgIndent <> loc i <+> text "=" <+> x <+> br) v <+>
     cgIndent <> cgBody ret sc
cgBody ret (SUpdate n e) = cgIndent <> cgBody ret e
cgBody ret (SProj e i)
   = ret $ cgVar e <> lbrack <> text (show (i + 1)) <> rbrack
cgBody ret (SCon _ t n args)
   = ret $ lbrack <> text (showSep "," ((show t) : (map (show . cgVar) args))) <> rbrack
cgBody ret (SCase _ e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar <> text "[0]" else scrvar in
          cgIndent <> text "case" <+> scr $+$ 
          text (showSep "\n" (map (show . (cgAlt ret scrvar)) alts)) $+$ 
          cgIndent <> end <+> br
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SChkCase e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar <> text "[0]" else scrvar in
          cgIndent <> text "case" <+> scr $+$
            text (showSep "\n" (map (show . (cgAlt ret scrvar)) alts)) $+$
            cgIndent <> end <+> br
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False

cgBody ret (SConst c) = ret $ (cgConst c)
cgBody ret (SOp op args) = ret $ cgOp op (map (show . cgVar) args)
cgBody ret SNothing = ret $ text "0"
cgBody ret (SError x) = ret $ text ("idris_error( " ++ show x ++ ")")
cgBody ret _ = ret $ text "idris_error(\"NOT IMPLEMENTED!!!!\")"

cgAlt :: (Doc -> Doc) -> Doc -> SAlt -> Doc
cgAlt ret scr (SConstCase t exp)
   = cgIndent <> when <+> text (show t) $+$ 
     cgIndent <> cgBody ret exp
cgAlt ret scr (SDefaultCase exp) = cgIndent <> when <> text "-1; else" $+$
                                   cgIndent <> cgBody ret exp
cgAlt ret scr (SConCase lv t n args exp)
   = cgIndent <> when <+> text (show t) $+$
     cgIndent <> project 1 lv args $+$ 
     cgIndent <> cgBody ret exp
   where project i v [] = text ""
         project i v (n : ns) = cgIndent <> loc v <> text " = " <> scr <> lbrack <> text (show i) <> rbrack $+$
                                cgIndent <> project (i + 1) (v + 1) ns

cgVar :: LVar -> Doc
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> Doc
cgConst (I i) = text $ show i
cgConst (Ch i) = text $ show (ord i)
cgConst (BI i) = text $ show i
cgConst (Str s) = text $ show s
cgConst TheWorld = text "0"
cgConst x | isTypeConst x = text "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: PrimFn -> [String] -> Doc
cgOp (LPlus (ATInt _)) [l, r] 
     = text $ "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LMinus (ATInt _)) [l, r] 
     = text $ "(" ++ l ++ " - " ++ r ++ ")"
cgOp (LTimes (ATInt _)) [l, r] 
     = text $ "(" ++ l ++ " * " ++ r ++ ")"
cgOp (LEq (ATInt _)) [l, r] 
     = text $ "idris_is_none(" ++ l ++ ", " ++ r ++ ")"
cgOp (LSLt (ATInt _)) [l, r] 
     = text $ "(" ++ l ++ " < " ++ r ++ ")"
cgOp (LSLe (ATInt _)) [l, r] 
     = text $ "(" ++ l ++ " <= " ++ r ++ ")"
cgOp (LSGt (ATInt _)) [l, r] 
     = text $ "(" ++ l ++ " > " ++ r ++ ")"
cgOp (LSGe (ATInt _)) [l, r] 
     = text $ "(" ++ l ++ " >= " ++ r ++ ")"
cgOp LStrEq [l,r] = text $ "idris_is_none(" ++ l ++ ",  " ++ r ++ ")"
cgOp LStrRev [x] = text $ x ++ ".reverse"
cgOp LStrLen [x] = text $ x ++ ".length"
cgOp LStrHead [x] = text $ x ++ "[0].ord"
cgOp LStrIndex [x, y] = text $ x ++ "[" ++ y ++ "].ord"
cgOp LStrTail [x] = text $ x ++ "[1..-1]"

cgOp (LIntStr _) [x] = text $ x ++ ".to_s"
cgOp (LChInt _) [x] = text $ x 
cgOp (LIntCh _) [x] = text $ x
cgOp (LSExt _ _) [x] = text $ x
cgOp (LTrunc _ _) [x] = text $ x
cgOp LWriteStr [_,str] = text $ "idris_writeStr(" ++ str ++ ")"
cgOp LReadStr [_] = text $ "idris_readStr()"
cgOp LStrConcat [l,r] = text $ "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp LStrCons [l,r] = text $ "idris_append(" ++ l ++ ".chr, " ++ r ++ ")"
cgOp op exps = text $ "idris_error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



