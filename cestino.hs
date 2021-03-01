

flatten :: Int -- Current column; 0 for first column
        -> [(Iseq, Int)] -- Work list
        -> String -- Result

flatten col ((INewline, indent) : seqs) = '\n' : (space indent) ++ (flatten indent seqs)

flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)


flatten _ [] = ""
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((IStr s, indent) : seqs) = s ++ flatten (col+lenght s) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2,indent) : seqs)


pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = iAppend (pprExpr e1) (iAppend (iStr " ") (pprAExpr e2))
--(pprExpr e1) 'iAppend' (iStr " ") 'iAppend' (pprAExpr e2)
pprExpr (ECase expr alts) = pprExpr expr
pprExpr (ELam defns expr) =

pprExpr (ELet isrec defns expr) = iConcat [ iStr keyword, iNewline,iStr " ",iIndent (pprDefns defns),iNewline,iStr "in ",pprExpr expr ]
                                  where
                                       keyword | not isrec = "let"
                                               | isrec = "letrec"

pprAExpr :: CoreExpr -> Iseq


pprDefns :: [(Name,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
                 where
                      sep = iConcat [ iStr ";", iNewline ]
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]
