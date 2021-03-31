--import Data.Heap
--import Data.Primitive.Addr
import System.IO
import ParseProg
import Heap
import Iseq
import Parse

type TiStack = [Addr]

--type Addr = Int

type TiDump = [TiStack]
initialTiDump = []

type TiHeap = Heap Node

type Tag = Int
type Arity = Int

data Node = NAp Addr Addr
             | NSupercomb Name [Name] CoreExpr
             | NNum Int
             | NInd Addr
             | NPrim Name Primitive
             | NData Int [Addr]
             | NMarked Node

data Primitive = Neg | Add | Sub | Mul | Div | PrimConstr Tag Arity 
                | If | Greater | GreaterEq | Less | LessEq | Eq | NotEq 
                | PrimCasePair | Abort | PrimCaseList | Stop | Print deriving Show

{-data Boolean = Falsez | Truez

Falsez = Pack{1,0}
Truez = Pack{2,0}-}

type TiGlobals = ASSOC Name Addr

-- STATISTICHEData.Heap

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s+1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

type TiState = ([Int], TiStack, TiDump, TiHeap, TiGlobals, TiStats)

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (output, stack, dump, heap, sc_defs, stats) = (output, stack, dump, heap, sc_defs, stats_fun stats)

preludeDefs :: CoreProgram
preludeDefs = [ ("I", ["x"], EVar "x"),
                ("K", ["x","y"], EVar "x"),
                ("K1",["x","y"], EVar "y"),
                ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
                ("compose", ["f","g","x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
                ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")),
                ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K")),
                ("snd",["p"],EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))]

extraPreludeDefs = [("False",[],EConstr 1 0),
                    ("True",[],EConstr 2 0),
                    ("&",["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False")),
                    ("|",["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y")),
                    ("xor", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y"))) (EVar "y")),
                    ("not", ["y"], EAp (EAp (EAp (EVar "if") (EVar "y")) (EVar "False")) (EVar "True")),
                    ("MkPair", [], EConstr 1 2),
                    ("Nil",[], EConstr 1 0),
                    ("Cons",[], EConstr 2 2),
                    ("head", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K")),
                    ("tail", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K1")),
                    ("printList",["xs"],EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "stop")) (EVar "printCons")),
                    ("printCons",["h","t"],EAp (EAp (EVar "print") (EVar "h")) (EAp (EVar "printList") (EVar "t")))
                   ]

primitives :: ASSOC Name Primitive
primitives = [ ("negate", Neg),
               ("+", Add),
               ("-", Sub), 
               ("*", Mul), 
               ("/", Div), 
               ("if", If), 
               (">", Greater), 
               (">=",GreaterEq), 
               ("<",Less), 
               ("<=",LessEq), 
               ("==",Eq), 
               ("/=",NotEq),
               ("casePair", PrimCasePair),
               ("abort", Abort),
               ("caseList", PrimCaseList),
               ("print", Print),
               ("stop", Stop)
             ]

compile :: CoreProgram -> TiState
compile program = (output, initial_stack, initialTiDump, initial_heap', globals, tiStatInitial)
                    where sc_defs = program ++ preludeDefs ++ extraPreludeDefs
                          (initial_heap, globals) = buildInitialHeap sc_defs
                          initial_stack = [addr]
                          address_of_main = aLookup globals "main" (error "main is not defined")
                          address_of_print = aLookup globals "printList" (error "print_list is not defined")
                          (initial_heap', addr) = hAlloc initial_heap (NAp address_of_print address_of_main)
                          output = []

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
                where
                     (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
                               where
                                    (heap', addr) = hAlloc heap (NPrim name prim)

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = (heap2, sc_addrs ++ prim_addrs)
                            where
                              (heap1, sc_addrs) = mapAccuml allocateSc hInitial sc_defs
                              (heap2, prim_addrs) = mapAccuml allocatePrim heap1 primitives                              

eval :: TiState -> [TiState]
eval state = state : rest_states
             where
             rest_states | tiFinal state = []
                         | otherwise = eval next_states
             next_states = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state | hSize heap > 40 = applyToStats tiStatIncSteps (gc state)
              | otherwise = applyToStats tiStatIncSteps state
              where (_, _, _, heap, _, _) = state
              
gc :: TiState -> TiState
gc (output, stack, dump, heap, globals, stats) = (output, stack, dump, cleaned_heap, globals, stats)
                                               where 
                                                     addrs = findRoots stack dump globals
                                                     marked_heap = foldl markFrom heap addrs 
                                                     cleaned_heap = scanHeap marked_heap

markFrom :: TiHeap -> Addr -> (TiHeap,Addr)
markFrom heap addr = case node of
                       (NMarked n) -> (heap,addr)
                       (NAp a1 a2) ->  (heap''',addr) where (heap',banana) = markFrom heap a1
                                                            (heap'',banana2) = markFrom heap' a2
                                                            heap''' = hUpdate heap'' addr (NMarked (NAp banana banana2))
                       (NInd a) -> (heap'', bananino) where (heap',bananino) = markFrom heap a
                                                --heap'' = hUpdate heap' addr (NMarked node)
                       (NData _ addrs) -> (hUpdate (foldl markFrom heap addrs) addr (NMarked node), addr)
                       nod -> (hUpdate heap addr (NMarked nod),addr)
                   where node = hLookup heap addr

scanHeap :: TiHeap -> TiHeap
scanHeap heap = foldl free heap xs
              where (_,_,xs) = heap

free :: TiHeap -> (Addr, Node) -> TiHeap
free heap x = case x of
                  (addr, NMarked n) -> hUpdate heap addr n
                  (addr, _) -> hFree heap addr

findRoots :: TiStack -> TiDump -> TiGlobals -> [Addr]
findRoots stack dump globals = findStackRoots stack ++ findDumpRoots dump ++ findGlobalRoots globals

markFromStack :: TiHeap -> TiStack -> (TiHeap,TiStack)--findStackRoots stack = stack
markFromStack heap stack = mapAccuml markFrom heap stack

markFromDump :: TiHeap -> TiDump -> (TiHeap,TiDump)
markFromDump dump = foldr (++) [] dump

markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap,TiGlobals)
markFromGlobals [] = []
markFromGlobals ((nm, addr) : xs) = addr : (findGlobalRoots xs)

remove_duplicates :: Eq a => [a] -> [a]
remove_duplicates [] = []
remove_duplicates (x:xs) = x: (remove_duplicates (confronto x xs))

confronto :: Eq a => a -> [a] -> [a]
confronto _ [] = []
confronto y (x:xs) | y == x    = confronto y xs
                   | otherwise = x: (confronto y xs)

tiFinal :: TiState -> Bool
tiFinal (output, [sole_addr], [], heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal (output, [sole_addr], dump, heap, globals, stats) = False
tiFinal (output, [], dump, heap, globals, stats) = True
tiFinal state = False -- Stack contains more than one item

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode (NData t a) = True
isDataNode _ = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
             where (output,stack, dump, heap, globals, stats) = state
                   dispatch (NNum n) = numStep state n
                   dispatch (NAp a1 a2) = apStep state a1 a2
                   dispatch (NSupercomb sc args body) = scStep state sc args body
                   dispatch (NInd a) = (output,a:drop 1 stack, dump, heap, globals, stats)
                   dispatch (NPrim nm pr) = primStep state pr
                   dispatch (NData _ _) = dataStep state

numStep :: TiState -> Int -> TiState
numStep (output, s1:s2:stack, dump, heap, globals, stats) _ = error "Stack contains more than one element"
numStep (output, stack, [], heap, globals, stats) _ = error "Empty dump!"
numStep (output, a:[], (s:dump), heap, globals, stats) _ = (output, s, dump, heap, globals, stats)  --error "Number applied as a function!"

dataStep :: TiState -> TiState
dataStep (output, (a:[]), (s:dump), heap, globals, stats) = (output, s, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep (output, stack, dump, heap, globals, stats) a1 a2 = gnammy node
                                                         where
                                                               gnammy (NInd a3) = (output, stack, dump, (hUpdate heap (head stack) (NAp a1 a3)), globals, stats)
                                                               gnammy _ = (output, a1 : stack, dump, heap, globals, stats)
                                                               node = hLookup heap a2


scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (output, stack, dump, heap, globals, stats) sc_name arg_names body = (output, new_stack, dump, new_heap, globals, stats)
                                                                          where new_stack = a_n : stack'
                                                                                new_heap = instantiateAndUpdate body a_n heap env
                                                                                (a_n:stack') = (drop (length arg_names) stack)
                                                                                env = arg_bindings ++ globals
                                                                                arg_bindings = zip arg_names (getargs heap stack)


primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state (div)
primStep state (PrimConstr t ar) = primConstr state (PrimConstr t ar)
primStep state If = primIf state 
primStep state Greater = primComp state (>)
primStep state GreaterEq = primComp state (>=)
primStep state Less = primComp state (<) 
primStep state LessEq = primComp state (<=)
primStep state Eq = primComp state (==) 
primStep state NotEq = primComp state (/=)
primStep state (PrimCasePair) = primCasePair state
primStep state (PrimCaseList) = primCaseList state
primStep state Abort = error "empty list"
primStep state Stop = primStop state
primStep state Print = primPrint state


primCasePair :: TiState -> TiState
primCasePair (output, stack, dump, heap, globals, stats) = case mk of 
                                                             (NData 1 [a,b]) -> (output, stack', dump, heap'', globals, stats) where 
                                                                                                                                     heap'' = hUpdate heap' (head stack') (NAp addr b)
                                                                                                                                     (heap',addr) = hAlloc heap (NAp f_addr a)
                                                             _               -> (output, [mk_addr], (drop 1 stack):dump, heap, globals, stats)
                                                         where 
                                                               mk = hLookup heap mk_addr
                                                               [mk_addr,f_addr] = getargs heap stack

                                                               stack' = drop 2 stack

primCaseList :: TiState -> TiState
primCaseList (output, stack, dump, heap, globals, stats) = case pack of
                                                             (NData 1 []) -> (output, stack', dump, heap', globals, stats) where
                                                                                                                                 heap' = hUpdate heap (head stack') (hLookup heap base_addr)
                                                             (NData 2 [a,b]) ->  (output, stack', dump, heap'', globals, stats) where
                                                                                                                                      heap'' = hUpdate heap' (head stack') (NAp addr b)
                                                                                                                                      (heap',addr) = hAlloc heap (NAp f_addr a)
                                                             _ -> (output, [pack_addr], (drop 1 stack):dump, heap, globals, stats)
                                                         where
                                                               pack = hLookup heap pack_addr
                                                               [pack_addr, base_addr, f_addr] = getargs heap stack

                                                               stack' = drop 3 stack


primConstr :: TiState -> Primitive -> TiState
primConstr (output, stack, dump, heap, globals, stats) (PrimConstr t ar) | length(addrs) == ar = (output, stack', dump, heap', globals, stats)
                                                                 | otherwise = error "not enough arguments"
                                                                   where 
                                                                        heap' = hUpdate heap (head stack') (NData t addrs)
                                                                        stack' = (drop ar stack)
                                                                        addrs = take ar (getargs heap stack)



primNeg :: TiState -> TiState
primNeg (output, (a:a1:stack), dump, heap, globals, stats) | isDataNode node = (output, (a1:stack), dump, hUpdate heap a1 (neg node), globals, stats)
                                                           | otherwise       = (output, (addr:[]), (a1:stack):dump, heap, globals, stats)
                                                           where 
                                                                 node = hLookup heap addr
                                                                 [addr] = getargs heap (a:a1:stack) -- ritorna più di un solo elemento!?
                                                                 neg (NNum n) = NNum (-n)
                                                           
primIf :: TiState -> TiState
primIf (output, (primif:a1:a2:a3:stack), dump, heap, globals, stats) = case cond of 
                                                                         (NData 1 []) -> (output, a_else:stack, dump, hUpdate heap a3 (hLookup heap a_else), globals, stats)
                                                                         (NData 2 []) -> (output, a_then:stack, dump, hUpdate heap a3 (hLookup heap a_then), globals, stats)
                                                                         _            -> (output, a_cond:[], (primif:a1:a2:a3:stack):dump, heap, globals, stats)
                                                                     where 
                                                                           cond = hLookup heap a_cond
                                                                           a_cond: a_then : a_else : [] = getargs heap (primif:a1:a2:a3:[])
                                                
{-
primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith ((a:a1:a2:stack), dump, heap, globals, stats) f | isDataNode node1 && isDataNode node2 = ((a2:stack), dump, hUpdate heap a2 (NNum (f (num node1) (num node2))), globals, stats)
                                                          | not (isDataNode node1) = ((addr1:[]), (a1:a2:stack):dump, heap, globals, stats)
                                                          | not (isDataNode node2) = ((addr2:[]), (a1:a2:stack):dump, heap, globals, stats)
                                                            where 
                                                                  node1 = hLookup heap addr1
                                                                  node2 = hLookup heap addr2
                                                                  [addr1,addr2] = getargs heap (a:a1:a2:stack)
                                                                  num (NNum n) = n
-}

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state f = primDyadic state (\(NNum x) (NNum y) -> NNum (f x y)) 
                                   
primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state f = primDyadic state (\(NNum x) (NNum y) -> if f x y then (NData 2 []) else (NData 1 []))
                                          
primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (output, (a:a1:a2:stack), dump, heap, globals, stats) f | isDataNode node1 && isDataNode node2 = (output, (a2:stack), dump, hUpdate heap a2 (f node1 node2), globals, stats)
                                                                   | not (isDataNode node1) = (output, (addr1:[]), (a1:a2:stack):dump, heap, globals, stats)
                                                                   | not (isDataNode node2) = (output, (addr2:[]), (a1:a2:stack):dump, heap, globals, stats)
                                                                   where 
                                                                         node1 = hLookup heap addr1
                                                                         node2 = hLookup heap addr2
                                                                         [addr1,addr2] = getargs heap (a:a1:a2:stack)

primStop :: TiState -> TiState
primStop (output, (a:[]), [], heap, globals, stats) = (output, [], [], heap, globals, stats)
primStop (output, (a:[]), dump, heap, globals, stats) = error "dump is not empty when calling stop"

primPrint :: TiState -> TiState
primPrint (output,(a:a1:a2:[]), [], heap, globals, stats) = case node of -- ⇒
                                                              NNum n -> (output ++ [n], b2_addr:[], [], heap, globals, stats)
                                                              _      
                                                                     | isDataNode node -> error "wrong type for a list member"
                                                                     | otherwise -> (output, (b1_addr:[]), ([a2]:[]), heap, globals, stats)
                                                          where node = hLookup heap b1_addr
                                                                [b1_addr, b2_addr] = getargs heap (a:a1:a2:[])

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

instantiateConstr tag arity heap env = hAlloc heap (NPrim "Pack" (PrimConstr tag arity))
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

instantiateAndUpdate :: CoreExpr --Body of supercombinator
                        -> Addr -- Address of node to update
                        -> TiHeap -- Heap before instantiation
                        -> ASSOC Name Addr -- Associate parameters to addresses
                        -> TiHeap -- Heap after instantiation

instantiateAndUpdate (ENum n) upd_addr heap env = hUpdate heap upd_addr (NNum n)

instantiateAndUpdate (EAp e1 e2) upd_addr heap env = hUpdate heap2 upd_addr (NAp a1 a2)
                                                     where (heap1, a1) = instantiate e1 heap env
                                                           (heap2, a2) = instantiate e2 heap1 env

instantiateAndUpdate (EVar v) upd_addr heap env = hUpdate heap upd_addr (NInd (aLookup env v (error ("Undefined name " ++ show v))))

instantiateAndUpdate (EConstr tag arity) upd_addr heap env = hUpdate heap upd_addr (NPrim "Pack" (PrimConstr tag arity))
instantiateAndUpdate (ELet isrec defs body) upd_addr heap env = instantiateAndUpdate body upd_addr heap1 env1
                                                              where (heap1,env1) = instantiateLet isrec defs body heap env

instantiateAndUpdate (ECase e alts) upd_addr heap env = error "Can’t instantiate case exprs"

--letrec a = b; b= c in a
--1. instantiate the right-hand side of each of the definitions in defs;
--2. augment the environment to bind the names in defs to the addresses of the newly constructed instances;
--3. call instantiate passing the augmented environment and the expression body.

showResults :: [TiState] -> [Char]
showResults states = iDisplay (iConcat [ iLayn (map showState states), showStats (last states)])

showState :: TiState -> Iseq
showState (output, stack, dump, heap, globals, stats) = iConcat [ showStack heap stack, iNewline ]

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
showNode (NPrim nm pr) = iAppend (iStr "NPrim ") (iStr nm)
showNode (NData t addrs) = iConcat [iStr "NData ", (iNum t), iStr " ", (iNum (length addrs))]

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space1 (4 - length str) ++ str)
                where str = show addr

showStats :: TiState -> Iseq
showStats (output, stack, dump, heap, globals, stats) = iConcat ([ iNewline, iNewline, iStr "Total number of steps = ", iNum (tiStatGetSteps stats), iNewline, iStr "Main = ["] ++ (map (\n -> iAppend (iNum n) (iStr ",")) output) ++ [iStr "]"])

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
a = pair x b ;fst p = p K
b = pair y a
in
fst (snd (snd (snd a))) ;
main = f 3 4-}

{-"main = letrec a = pair x b ; b = pair y a in fst (snd (snd (snd a)))"-}

{- 
x = 5
main = x

NPrim Add
a:b:a1:[] d h[a:NPrim Add; a1:NAp b c; b:NAp a d]

[([("main",[],EAp (EAp (EAp (EVar "if") (EAp (EAp (EVar "&") (EVar "True")) (EVar "True"))) (ENum 5)) (ENum 6))],"")]
"fac n = if (n == 0) 1 (n * fac (n-1)) ; main = fac 3"

mkpair = pack{1,2}
ciao = mkpair 5 6
main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)))
main = fst (MkPair 5 6)

NPrim casePair
a:ap1:ap2:stack d h[a: NPirm casePair, ap1: Nap nprim arg, ap2: NAp ap1 arg2]
getargs stack

NPrim caseList
a:a1:a2:a3:[] d h[a:Nprim caseList, a1: Nap nprim arg, a2: NAp a1 arg2, a3: Nap a2 arg3]

EAp (EAp (EConstr 1 2) (ENum 5)) (ENum 6))
pack{1,2} 5 6

Eap (Eap (casePair) (NData)) (f)
f a b

main = S K K 3

main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)))

                ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1")),
                ("snd",["p"],EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))]

   1) Stk [   1: NSupercomb main
            ]
      
   2) Stk [   1: NAp    8   41 (NAp 9 40)
            ]
      
   3) Stk [   8: NSupercomb fst
              1: NAp    8   41 (NAp 9 40)
            ]
      
   4) Stk [   1: NAp   42    4 (NSupercomb K1)
            ]
      
   5) Stk [  42: NAp   29   41 (NAp 9 40)
              1: NAp   42    4 (NSupercomb K1)
            ]
      
   6) Stk [  29: NPrim casePair
             42: NAp   29   41 (NAp 9 40)
              1: NAp   42    4 (NSupercomb K1)
            ]
      
   7) Stk [  41: NAp    9   40 (NAp 8 39)
            ]
      
   8) Stk [   9: NSupercomb snd
             41: NAp    9   40 (NAp 8 39)
            ]
      
   9) Stk [  41: NAp   43    4 (NSupercomb K1)
            ]
      
  10) Stk [  43: NAp   29   40 (NAp 8 39)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  11) Stk [  29: NPrim casePair
             43: NAp   29   40 (NAp 8 39)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  12) Stk [  40: NAp    8   39 (NAp 37 38)
            ]
      
  13) Stk [   8: NSupercomb fst
             40: NAp    8   39 (NAp 37 38)
            ]
      
  14) Stk [  40: NAp   44    4 (NSupercomb K1)
            ]
      
  15) Stk [  44: NAp   29   39 (NAp 37 38)
             40: NAp   44    4 (NSupercomb K1)
            ]
      
  16) Stk [  29: NPrim casePair
             44: NAp   29   39 (NAp 37 38)
             40: NAp   44    4 (NSupercomb K1)
            ]
      
  17) Stk [  39: NAp   37   38 (NNum 4)
            ]
      
  18) Stk [  37: NAp   16   36 (NAp 31 35)
             39: NAp   37   38 (NNum 4)
            ]
      
  19) Stk [  16: NSupercomb MkPair
             37: NAp   16   36 (NAp 31 35)
             39: NAp   37   38 (NNum 4)
            ]
      
  20) Stk [  16: NPrim Pack
             37: NAp   16   36 (NAp 31 35)
             39: NAp   37   38 (NNum 4)
            ]
      
  21) Stk [  39: NData 1
            ]
      
  22) Stk [  44: NAp   29   39 (NData 1)
             40: NAp   44    4 (NSupercomb K1)
            ]
      
  23) Stk [  29: NPrim casePair
             44: NAp   29   39 (NData 1)
             40: NAp   44    4 (NSupercomb K1)
            ]
      
  24) Stk [  40: NAp   45   38 (NNum 4)
            ]
      
  25) Stk [  45: NAp    4   36 (NAp 31 35)
             40: NAp   45   38 (NNum 4)
            ]
      
  26) Stk [   4: NSupercomb K1
             45: NAp    4   36 (NAp 31 35)
             40: NAp   45   38 (NNum 4)
            ]
      
  27) Stk [  40: NInd   38
            ]
      
  28) Stk [  38: NNum 4
            ]
      
  29) Stk [  43: NAp   29   40 (NInd   38)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  30) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  31) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  32) Stk [  38: NNum 4
            ]
      
  33) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  34) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  35) Stk [  38: NNum 4
            ]
      
  36) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  37) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  38) Stk [  38: NNum 4
            ]
      
  39) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  40) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  41) Stk [  38: NNum 4
            ]
      
  42) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  43) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  44) Stk [  38: NNum 4
            ]
      
  45) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  46) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  47) Stk [  38: NNum 4
            ]
      
  48) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  49) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  50) Stk [  38: NNum 4
            ]
      
  51) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  52) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  53) Stk [  38: NNum 4
            ]
      
  54) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  55) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  56) Stk [  38: NNum 4
            ]
      
  57) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  58) Stk [  29: NPrim casePair
             43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  59) Stk [  38: NNum 4
            ]
      
  60) Stk [  43: NAp   29   38 (NNum 4)
             41: NAp   43    4 (NSupercomb K1)
            ]
      
  61) Stk [  29: NPrim casePair
             43: N^CApInterrupted.
*Main> 


Pack{2,2} 1 2 3 4 5 6
NData 5 [1,2,3,4,5,6]

NData 2 [1,x]
NData 2 [2,x1]
NDat

listone = Pack{2,2} 1 (Pack{2,2} 2 (Pack{2,2} 3 Pack{1,0}))

-}