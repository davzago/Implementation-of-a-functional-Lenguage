module ParseProg where
import Control.Applicative
import Parse
type Name = String
type Def a = (a, Expr a)
type Alter a = (Int, [a], Expr a)
data IsRec = NonRecursive | Recursive deriving (Eq,Show)
type Program a = [ScDef a]
type CoreProgram = Program Name
type ScDef a = (Name, [a], Expr a)
type CoreScDefn = ScDef Name
type CoreExpr = Expr Name

data Expr a
  =  EVar Name
   | ENum Int
   | EConstr Int Int
   | EAp (Expr a) (Expr a)
   | ELet
        IsRec
        [(a,Expr a)]
        (Expr a)
   | ECase 
        (Expr a)
        [Alter a]
   | ELam [a] (Expr a)
    deriving Show

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do char ';'
                  ps <- parseProg
                  return (p:ps)
                <|> return [p]

parseScDef :: Parser (ScDef Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                char '='
                body <- parseExpr -- call to parseExpr
                return (v, pf, body)

parseExpr :: Parser (Expr Name)
parseExpr = parseLet <|> parseLetrec <|> parseCase <|> parseLam <|> parseExpr1

parseAExpr :: Parser (Expr Name)
parseAExpr = do parseKeyword
                do e <- parseVar
                   return (EVar e)
                 <|> do x <- natural
                        return (ENum x)
                 <|> do symbol "Pack{"
                        n <- natural
                        symbol ","
                        m <- natural 
                        symbol "}"
                        return (EConstr n m) 
                 <|> do symbol "("
                        e <- parseExpr
                        symbol ")"
                        return e
                 <|> empty
                
parseLet :: Parser (Expr Name)
parseLet = do symbol "let"
              d <- parseDefns
              symbol "in"
              e <- parseExpr
              return (ELet NonRecursive d e)

parseLetrec :: Parser (Expr Name)
parseLetrec = do symbol "letrec"
                 d <- parseDefns
                 symbol "in"
                 e <- parseExpr
                 return (ELet Recursive d e)

parseDefn :: Parser (Def Name)
parseDefn = do v <- identifier
               symbol "="
               e <- parseExpr
               return (v,e)

parseDefns :: Parser ([Def Name])
parseDefns = do d <- parseDefn
                ds <- many (do symbol ";" 
                               parseDefn)
                return (d:ds)

parseCase :: Parser (Expr Name)
parseCase = do symbol "case"
               e <- parseExpr
               symbol "of"
               a <- parseAlts
               return (ECase e a)

parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              n <- natural
              symbol ">"
              as <- many (do identifier)
              symbol "->"
              e <- parseExpr
              return (n,as,e)

parseAlts :: Parser ([Alter Name])
parseAlts = do a <- parseAlt
               as <- many (do symbol ";"
                              parseAlt)
               return (a:as)

parseLam :: Parser (Expr Name)
parseLam = do symbol "\\"
              a <- identifier
              as <- many (do identifier)
              symbol "."
              e <- parseExpr
              return (ELam (a:as) e)

parseVar:: Parser String
parseVar = token var

var :: Parser String
var = do x <- letter
         xs <- many (alphanum <|> (char '_'))
         return (x:xs)

parseExpr1:: Parser (Expr Name)
parseExpr1 = do e2 <- parseExpr2 
                do symbol "|"
                   e1 <- parseExpr1
                   return (EAp (EAp (EVar "|") e2) e1)
                 <|> return e2

parseExpr2:: Parser (Expr Name)
parseExpr2 = do e3 <- parseExpr3 
                do symbol "&"
                   e2 <- parseExpr2
                   return (EAp (EAp (EVar "&") e3) e2)
                 <|> return e3

parseExpr3:: Parser (Expr Name)
parseExpr3 = do e4 <- parseExpr4
                do r <- parseRelop
                   e4' <- parseExpr4
                   return (EAp (EAp r e4) e4')
                 <|> return e4

parseRelop :: Parser (Expr Name)
parseRelop = do symbol "<"
                return (EVar "<")
             <|> do symbol "<="
                    return (EVar "<=")
             <|> do symbol "=="
                    return (EVar "==")
             <|> do symbol "~="
                    return (EVar "~=")
             <|> do symbol ">="
                    return (EVar ">=")
             <|> do symbol ">"
                    return (EVar ">")

parseExpr4:: Parser (Expr Name)
parseExpr4 = do e5 <- parseExpr5
                do symbol "+"
                   e4 <- parseExpr4
                   return (EAp (EAp (EVar "+") e5) e4)
                 <|> do symbol "-"
                        e5' <- parseExpr5
                        return (EAp (EAp (EVar "-") e5) e5')
                 <|> return e5

parseExpr5 :: Parser (Expr Name)
parseExpr5 = do e6 <- parseExpr6
                do symbol "*"
                   e5 <- parseExpr5
                   return (EAp (EAp (EVar "*") e6) e5)
                 <|> do symbol "/"
                        e6' <- parseExpr6
                        return (EAp (EAp (EVar "/") e6) e6')
                 <|> return e6

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do (a:as) <- some parseAExpr
                return (foldl (\x y -> EAp x y) a as)

parseKeyword :: Parser String
parseKeyword = P(\inp -> case parse (symbol "in" <|> symbol "case" <|> symbol "let" <|> symbol "letrec" <|> symbol "of" <|> empty) inp of
                            [] -> [("",inp)]
                            [(v,out)] -> [])