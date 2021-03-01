--import Data.Heap
--import Data.Primitive.Addr
import System.IO
import ParseProg
import Heap
import Iseq
import Parse

type TiStack = [Addr]

--type Addr = Int

data TiDump = DummyTiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node = NAp Addr Addr
             | NSupercomb Name [Name] CoreExpr
             | NNum Int
             | NInd Addr

type TiGlobals = ASSOC Name Addr

-- STATISTICHEData.Heap

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s+1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)

preludeDefs :: CoreProgram
preludeDefs = [ ("I", ["x"], EVar "x"),
                ("K", ["x","y"], EVar "x"),
                ("K1",["x","y"], EVar "y"),
                ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
                ("compose", ["f","g","x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
                ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")),
                ("bau",["x","y"],EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))]

extraPreludeDefs = []

compile :: CoreProgram -> TiState
compile program = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
                    where sc_defs = program ++ preludeDefs ++ extraPreludeDefs
                          (initial_heap, globals) = buildInitialHeap sc_defs
                          initial_stack = [address_of_main]
                          address_of_main = aLookup globals "main" (error "main is not defined")

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
                where
                     (heap', addr) = hAlloc heap (NSupercomb name args body)

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs

eval :: TiState -> [TiState]
eval state = state : rest_states
             where
             rest_states | tiFinal state = []
                         | otherwise = eval next_states
             next_states = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state



tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False -- Stack contains more than one item

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
             where (stack, dump, heap, globals, stats) = state
                   dispatch (NNum n) = numStep state n
                   dispatch (NAp a1 a2) = apStep state a1 a2
                   dispatch (NSupercomb sc args body) = scStep state sc args body
                   dispatch (NInd a) = (a:drop 1 stack, dump, heap, globals, stats)

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body = (new_stack, dump, new_heap', globals, stats)
                                                                    where new_stack = result_addr : stack'
                                                                          new_heap' = hUpdate new_heap a_n (NInd result_addr)
                                                                          (a_n:stack') = (drop (length arg_names) stack)
                                                                          (new_heap, result_addr) = instantiate body heap env
                                                                          env = arg_bindings ++ globals
                                                                          arg_bindings = zip arg_names (getargs heap stack)

-- now getargs since getArgs conflicts with Gofer standard.prelude
getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map get_arg stack
                          where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -- Body of supercombinator
               -> TiHeap -- Heap before instantiation
               -> ASSOC Name Addr -- Association of names to addresses
               -> (TiHeap, Addr) -- Heap after instantiation, and address of root of instance   
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
                        where (heap1, a1) = instantiate e1 heap env
                              (heap2, a2) = instantiate e2 heap1 env

instantiate (EVar v) heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))

instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiate body heap1 env1
                                             where (heap1,env1) = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can’t instantiate case exprs"

instantiateConstr tag arity heap env = error "Can’t instantiate constructors yet"
{-
instantiateLet isrec [] body heap env = instantiate body heap env --3
instantiateLet isrec ((v,expr):defs) body heap env = instantiateLet isrec defs body heap1 env1
                                                                 where (heap1, addr) = instantiate expr heap env1 --1
                                                                       env1 = (v,addr) : env --2
                                                                       -}

instantiateLet isrec [] body heap env = (heap,env)
instantiateLet isrec ((v,expr):defs) body heap env = (heap2, env2) 
                                                                 where (heap1, env1) = instantiateLet isrec defs body heap (if isrec == NonRecursive then env else env2)
                                                                       env2 = (v,addr) : env --2
                                                                       (heap2, addr) = instantiate expr heap1 env1 --1

--letrec a = b; b= c in a
--1. instantiate the right-hand side of each of the definitions in defs;
--2. augment the environment to bind the names in defs to the addresses of the newly constructed instances;
--3. call instantiate passing the augmented environment and the expression body.

showResults :: [TiState] -> [Char]
showResults states = iDisplay (iConcat [ iLayn (map showState states),showStats (last states)])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) = iConcat [ showStack heap stack, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack = iConcat [iStr "Stk [",iIndent (iInterleave iNewline (map show_stack_item stack)),iStr " ]"]
                     where show_stack_item addr = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr)]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) = iConcat [iStr "NAp ", showFWAddr fun_addr, iStr " ", showFWAddr arg_addr, iStr " (", showNode (hLookup heap arg_addr), iStr ")"]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", showAddr a1,
                                 iStr " ", showAddr a2 ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = iAppend (iStr "NNum ") (iNum n)
showNode (NInd a) = iAppend (iStr "NInd ") (showFWAddr a)


showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
-- Show address in field of width 4
showFWAddr addr = iStr (space1 (4 - length str) ++ str)
                where str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) = iConcat [ iNewline, iNewline, iStr "Total number of steps = ", iNum (tiStatGetSteps stats)]

--showResults (eval (compile (parseProg "main = S K K 3"))) 

readF :: IO String
readF = do inh <- openFile "input.txt" ReadMode
           prog <- readloop inh
           hClose inh
           return prog

main :: IO (Program Name)
main = do inp <- readF
          return (comp (parse parseProg inp)) --here you call parseProg

comp :: [(Program Name, Name)] -> Program Name
comp [] = error "no parse"
comp [(e,[])] = e
comp [(_,a)] = error ("doesn't use all input"++ a)

readloop inh = do ineof <- hIsEOF inh
                  if ineof 
                      then return []
                       else do
                             x <- hGetLine inh
                             xs <- readloop inh
                             return (x ++ xs)


--(stack, dump, heap, globals, stats) =  compile . comp . parse parseProg
runProg = putStr . showResults . eval . compile . comp . parse parseProg
--ciauz inp = showResults [compile . comp . (parse parseProg inp)]

{-putStr(runProg "main = s k k 3")
   1) Stk [   1: NSupercomb main
            ]
   2) Stk [  11: NAp    9   10 (NNum 3)
            ]
   3) Stk [   9: NAp    8    3 (NSupercomb k)
             11: NAp    9   10 (NNum 3)
            ]
   4) Stk [   8: NAp    5    3 (NSupercomb k)
              9: NAp    8    3 (NSupercomb k)
             11: NAp    9   10 (NNum 3)
            ]
   5) Stk [   5: NSupercomb s
              8: NAp    5    3 (NSupercomb k)
              9: NAp    8    3 (NSupercomb k)
             11: NAp    9   10 (NNum 3)
            ]
   6) Stk [  14: NAp   12   13 (NAp 3 10)
            ]
   7) Stk [  12: NAp    3   10 (NNum 3)
             14: NAp   12   13 (NAp 3 10)
            ]
   8) Stk [   3: NSupercomb k
             12: NAp    3   10 (NNum 3)
             14: NAp   12   13 (NAp 3 10)
            ]
   9) Stk [  10: NNum 3]
Total number of steps = 8-}

{-(EVar "s")
(EAp (EVar "s") (EVar "k"))
m(EAp (EAp (EVar "s") (EVar "k")) (EVar "k"))
EAp (EAp (EAp (EVar "s") (EVar "k")) (EVar "k")) (ENum 3))


main = s k k 3
                ("k", ["x","y"], EVar "x"),

                ("s", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
                [("s",["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))]

s f g x = (f x) (g x)
k x y = x

("bau",["x","y"],EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))

main = let x = 5; y = 6 in x
[([("main",[],ELet NonRecursive [("x",EAp (EAp (EVar "+") (ENum 5)) (ENum 6)),("y",ENum 6)] (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))],"")]

("x",EAp (EAp (EVar "+") (ENum 5)) (ENum 6))
("y",ENum 6)-}

--"pair x y f = f x y ; fst p = p K ; snd p = p K1 ; f x y = letrec a = pair x b ; b = pair y a in fst (snd (snd (snd a))) ;main = f 3 4"
--"pair x y f = f x y ; fst p = p K ; snd p = p K1 ; f x y = let a = pair x b in a ;main = f 3 4"

{-pair x y f = f x y ;
fst p = p K ;
snd p = p K1 ;
f x y = letrec
a = pair x b ;
b = pair y a
in
fst (snd (snd (snd a))) ;
main = f 3 4-}

{-"main = letrec a = pair x b ; b = pair y a in fst (snd (snd (snd a)))"-}
{-
f x y = x+y
ciao x = f x 5
main = ciao 5


id x = x ; main = twice twice id 3
-}