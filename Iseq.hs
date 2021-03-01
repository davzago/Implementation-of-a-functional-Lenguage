module Iseq where

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iNil :: Iseq -- The empty iseq
iNil = INil

iStr :: String -> Iseq -- Turn a string into an iseq
iStr str = IStr str
--iStr = iConcat . intersperse INewline . map IStr . lines

iAppend :: Iseq -> Iseq -> Iseq -- Append two iseqs
iAppend seq1 seq2 = IAppend seq1 seq2

iNewline :: Iseq -- New line with indentation
iNewline = INewline

iIndent :: Iseq -> Iseq -- Indent an iseq
iIndent seq = IIndent seq

iDisplay :: Iseq -> String -- Turn an iseq into a string
iDisplay seq = flatten 0 [(seq,0)]

iConcat :: [Iseq] -> Iseq
--iConcat [] = iNil
--iConcat (x:xs) = iAppend x (iConcat xs)

iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave v (x:xs) = iAppend x (iAppend v (iInterleave v xs))
--iInterleave sep seqs = foldr iAppend iNil (intersperse sep seqs)

flatten :: Int -- Current column; 0 for first column
        -> [(Iseq, Int)] -- Work list
        -> String -- Result

space1 :: Int -> String
space1 0 = ""
space1 n = ' ' : space1 (n-1)

flatten _ [] = ""
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((IStr s, indent) : seqs) = s ++ flatten (col+length s) seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2,indent) : seqs)
flatten col ((INewline, indent) : seqs) = '\n' : (space1 indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)


iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (space1 (width - length digits) ++ digits)
               where digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
             where lay_item (n, seq) = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]
