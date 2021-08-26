# Template Instantiation

Il progetto che abbiamo realizzato è basato sul secondo capitolo del libro "implementing functional lenguages", in particolare ci siamo focalizzati su un interprete basato su template instantiation. L'obbiettivo della nostra implementazione è quello di eseguire un programma scritto in Core Language. Ogni programma, scritto usando un linguaggio funzionale, è formato da una o più espressioni le quali devono essere valutate. Ciascuna espressione è rappresentata da un grafo e per valutarla bisogna fare una serie di riduzioni fino a raggiungere la forma normale.

## Componenti del programma

Di seguito descriviamo quali sono le componenti del programma e a cosa servono.

### Parser

Il parser è il componente che si occupa di elaborare il programma testuale preso come input e di restituire delle espressioni, questo passaggio è fondamentale per controllare che il programma non presenti errori sintattici.

### Compilatore

Il compilatore prende in input la lista di espressioni fornite dal parser e si occupa di creare lo stato iniziale del programma, lo stato consiste in:

- **Stack:** Una pila di indirizzi, ognuno dei quali identifica un nodo nell'*heap*;
- **Dump:** Una pila di stack che ha come obbiettivo quello di ricordare quali sono le valutazioni precedenti a cui ritornare quando la valutazione corrente è terminata;
- **Heap:** Una collezione di coppie chiave valore dove la chiave è un indirizzo e il valore è un nodo che identifica la tipologia di una espressione;
- **Globals:** Una collezione che collega il nome di una definizione con il suo indirizzo nello heap;

Lo stato iniziale è formato da uno stack che contiene l'indirizzo del main (definizione obbligatoria per ogni programma), dump vuoto, heap contenente tutte le definizioni iniziali, che verranno spiegate in seguito, infine globals contiene il nome di ogni definizione con associato il suo indirizzo nello heap.


### Valutatore

Per valutare il programma abbiamo utilizzato uno state transition system, questo significa che a meno di errori, partendo da uno stato iniziale il valutatore compie dei passi fino ad arrivare ad uno stato finale. Per ogni stato il sistema compie un passo dettato da delle regole di transizione.

## Implementazione

Nella pratica il valutatore controlla a che tipo di nodo corrisponde l'indirizzo in cima allo stack e applica la regola di transizione adeguata, i possibili tipi di nodo sono:

~~~ haskell
data Node = NAp Addr Addr
             | NSupercomb Name [Name] CoreExpr
             | NNum Int
             | NInd Addr
             | NPrim Name Primitive
             | NData Int [Addr]
             | NMarked Node
~~~

- NAp rappresenta l'applicazione di funzione, questo nodo contiene l'indirizzo della funzione e dell'argomento
- NSupercomb rappresenta una definzione 
- NNum rappresenta un numero intero
- NInd rappresenta una indirezione
- NPrim rappresenta una operazione primitiva 
- NData rappresenta un oggetto
- NMarked rappresenta un nodo marchiato che non deve essere eliminato dal garbage collector

Quando il compilatore crea l'heap iniziale inserisce tutte le definizioni del programma, del prelude e dell'extraprelude sotto forma di nodi di tipo NSupercomb, e le operazioni primitive sotto forma di nodi di tipo NPrim.

Di seguito mostriamo un esempio di esecuzione del programma "main = S K K 3" e discutiamo la sua esecuzione andando a guardare lo stack tra uno step e l'altro. 

~~~ 
   1) Stk [   1: NSupercomb main
            ]

   2) Stk [   1: NAp   42   43 (NNum 3)
            ]
      
   3) Stk [  42: NAp   41    3 (NSupercomb K)
              1: NAp   42   43 (NNum 3)
            ]
      
   4) Stk [  41: NAp    5    3 (NSupercomb K)
             42: NAp   41    3 (NSupercomb K)
              1: NAp   42   43 (NNum 3)
            ]
      
   5) Stk [   5: NSupercomb S
             41: NAp    5    3 (NSupercomb K)
             42: NAp   41    3 (NSupercomb K)
              1: NAp   42   43 (NNum 3)
            ]
      
   6) Stk [   1: NAp   40   44 (NAp 3 43)
            ]
      
   7) Stk [  40: NAp    3   43 (NNum 3)
              1: NAp   40   44 (NAp 3 43)
            ]
      
   8) Stk [   3: NSupercomb K
             40: NAp    3   43 (NNum 3)
              1: NAp   40   44 (NAp 3 43)
            ]
      
   9) Stk [  1: NNum 3
            ]
   Total number of steps = 8
~~~

La funzione *compile* per prima cosa mette in cima allo stack l'indirizzo del main, su questo stato quindi viene chiamata la funzione *eval* che procede a chiamare *step* finché non si ottiene uno stato finale, ogni chiamata di *step* chiamerà la funzione adeguata. In questo caso visto che il main è un supercombinator viene chiamata la funzione *scStep* che istanzia nello heap il corpo del supercombinator utilizzando la funzione *istantiateAndUpdate*, questa prende in input l'indirizzo del nodo rappresentante nello heap il supercombinator e verrà utilizzato per aggiornare il suddetto nodo con quello del body istanziato.

~~~
("main",[],EAp (EAp (EAp (EVar "S") (EVar "K")) (EVar "K")) (ENum 3))
~~~

Durante l'istanziazione del body se viene trovata una espressione EVar questa viene cercata nella lista di associazione nome-indirizzo (enviroment) e in questo caso viene riconosciuta come supercombinator.
Una volta istanziato il supercombinator si nota che nello stack è presente una applicazione dunque viene chiamata la funzione *apStep* che descatola il suo primo argomento, questo viene fatto da 2) a 5) dove si trova il supercombinator S in cima allo stack, in questo stato viene dunque chiamata nuovamente *scStep* che procede ad istanziare il corpo di S definito in questo modo nelle definizioni del prelude:

~~~
("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
~~~

In pratica la funzione S prende due funzioni ed un argomento e crea questa applicazione : f x (g x), visto che si tratta di un' applicazione viene usata nuovamente *apStep* che cerca l'applicazione outermost fino a quando trova K che è un super combinator definito in questo modo:

~~~
("K", ["x","y"], EVar "x")
~~~


Dunque il body di questo supercombinator è semplicemente una Evar x che per mano di *instantiateAndUpdate* viene collegata all'argomento corretto da *getargs*, ovvero NNum 3, la radice viene quindi aggiornata a questo valore.
Il secondo argomento della funzione non viene valutato in quanto non presente nel body, questo perché il linguaggio è lazy.
Nella successiva chiamata di step *TiFinal* ci dice che ci troviamo in uno stato finale in quanto nello stack c'è un solo indirizzo di un numero e il dump è vuoto quindi la valutazione termina.  

### Aritmetica

Per implementare le operazioni aritmetiche utilizziamo le primitive:

~~~ haskell
data Primitive = Neg | Add | Sub | Mul | Div | PrimConstr Tag Arity 
                | If | Greater | GreaterEq | Less | LessEq | Eq | NotEq 
                | PrimCasePair | Abort | PrimCaseList | Stop | Print
~~~

Per prima cosa creiamo una lista di associazioni nome-primitiva:

~~~ haskell
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
~~~

EAP (EAP (+ 5  6) )

((+ (+ 5 6)) 7)



Il compilatore attraverso la chiamata *buildInitialHeap* alloca nello heap tutte le primitive utilizzando il tipo di dato NPrim, nel caso della somma il nodo allocato sarà NPrim '+' Add. Inoltre TiGlobals conterrà l'associazione nome indirizzo che permette di mappare il simbolo '+' all'indirizzo del nodo corrispondente nello heap, in questo modo quando il parser ritornerà l'espressione EVar '+' potremo recuperare nello heap il nodo della corrispondente primitiva.  

Di seguito mostriamo un esempio di esecuzione del programma "main = 3 + 2"

~~~ 
) Stk [   1: NSupercomb main
            ]
~~~
Qui viene applicato *scStep* in quanto in cima allo stack abbiamo un supercombinator.
~~~
      
   1) Stk [   1: NAp   42   43 (NNum 2)
            ]
~~~
Viene poi applicata la regola per le applicazioni usando *apStep* che non fa altro che mettere in cima allo stack il primo argomento dell'applicazione.
~~~

   2) Stk [  42: NAp   24   41 (NNum 3)
              1: NAp   42   43 (NNum 2)
            ]
~~~
apStep viene applicato nuovamente.
~~~
      
   3) Stk [  24: NPrim +
             42: NAp   24   41 (NNum 3)
              1: NAp   42   43 (NNum 2)
            ]
~~~
Visto che abbiamo trovato la primitiva '+' applichiamo la regola corrispondete chiamando *primStep* che a sua volta chiamerà *primArith state +* che si occupa di recuperare gli argomenti delle applicazioni di indirizzo 42 e 1, ossia i numeri 3 e 2 per poi applicarvi l'operatore + e sostituire all'indirizzo 1 il risultato. 
~~~
      
   4) Stk [   1: NNum 5
            ]
      
Total number of steps = 4
~~~

Quindi per ciascuna operazione viene usata la funzione *primStep* che a seconda della primitiva che viene trovata in cima allo stack decide cosa fare. In generale per le operazioni aritmetiche quello che accade è che se gli argomenti dell'operazione sono valutati si procede con l'operazione altrimenti si procede a valutare gli argomenti mettendo lo stack corrente nel dump e mettendo in cima allo stack l'argomento da valutare.
Le primitive sono state utilizzate anche per implementare l'If attraverso la funzione primIf, operazioni booleane con la funzione primComp, e per costruire oggetti come vedremo successivamente.

### Let e LetRec
Tra le varie istanziazioni di espressioni la più difficoltosa è quella di let, specialmente nel caso ricorsivo quando alcune definizioni possono formare dei cicli. Il problema è che quando si sta istanziando una definizione potrebbe essere necessaria l'associazione nome indirizzo di altre definizioni non ancora istanziate. Questo problema è stato risolto grazie al fatto che haskell è lazy, quindi nel caso ricorsivo quando si sta allocando una definizione viene passata la lista di associazioni nome indirizzo aumentata con tutte le definizioni anche se effettivamente nessuna di esse è ancora stata creata. In un linguaggio non lazy sarebbe stato necessario fare questa cosa a mano tenendo degli indirizzi da parte per poi poter allocare la definizione.

Andiamo ora a vedere un esempio di esecuzione della seguente istruzione: *main = let x = 5; y = 6 in x + y* dove il let viene usato in modo non ricorsivo.

~~~
1) Stk [   1: NSupercomb main
            ]
    
2) Stk [   1: NAp   43   42 (NNum 6)
         ]
   
3) Stk [  43: NAp   24   41 (NNum 5)
           1: NAp   43   42 (NNum 6)
         ]
   
4) Stk [  24: NPrim +
          43: NAp   24   41 (NNum 5)
           1: NAp   43   42 (NNum 6)
         ]
   
5) Stk [   1: NNum 11
         ]
      
~~~
L'esecuzione riportata è praticamente identica a quella della somma mostrata precedentemente, questo perchè mentre l'operazione è sempre la stessa, ovvero una somma, ciò che viene gestito in modo leggermente diverso sono le variabili. In questo caso infatti il programma usa il costrutto let che permette di creare delle definizioni che possono essere utilizzate all'interno del body. Il processo di assegnazione non è visibile guardando lo stack. 

Per prima cosa le definizioni vanno istanziate, questo viene fatto utilizzando il caso Let di InstantiateAndUpdate, nel nostro esempio le definizioni sono x = 5 e y = 6, si procede dunque a istanziare ciascuna definizione e successivamente ad aggiungere all'enviroment locale l'associazione nome della definizione e indirizzo del nodo risultante dall'istanziazione della definizione. 
Una volta istanziate le definizioni si procede con l'istanziazione e valutazione del body del costrutto let, ossia nel nostro esempio *x + y*.

Vediamo ora un altro esempio, dove viene mostrato chiaramente l'esecuzione del programma: *main = let y = 4 + 3; x = 4 in x+y*
~~~

   1) Stk [   1: NSupercomb main
            ]
~~~
In questa prima fase viene istanziato il corpo del main, ossia il costrutto let, nel fare ciò si procede ad istanziare tutte le sue definizioni, nel nostro caso y = 4 + 3, x = 4.
~~~
      
   2) Stk [   1: NAp   46   44 (NAp 42 43)
            ]
~~~
Viene istanziato il body del costrutto Let, e se ne inizia la valutazione eseguendo l'istruzione apStep che applica la regola dell'applicazione, andando a mettere sulla cima dello stack il nodo di indirizzo 46.
~~~
      
   3) Stk [  46: NAp   24   45 (NNum 4)
              1: NAp   46   44 (NAp 42 43)
            ]
~~~
Viene applicato apStep al nodo di indirizzo 46.
~~~
      
   4) Stk [  24: NPrim +
             46: NAp   24   45 (NNum 4)
              1: NAp   46   44 (NAp 42 43)
            ]
~~~
Ora primDyadic controlla se gli argomenti dei NAp di indirizzo 46 e 1, che sono rispettivamente i nodi di indirizzo 45 e 44, siano stati valutati completamente, nel nostro esempio il nodo di indirizzo 44 non è stato valutato, e corrisponde a *4 + 3*, questo succede in quanto il linguaggio è lazy, ossia valuta un'espressione solo quando è strettamente necessario. Si procede quindi a spostare lo stack corrente nel dump e inserire l'indirizzo del nodo che si vuole valutare nello stack appena svuotato. 
~~~
      
   5) Stk [  44: NAp   42   43 (NNum 3)
            ]
~~~
Ora abbiamo nello stack l'indirizzo del nodo che corrisponde all'espressione *4 + 3*, viene applicato apStep al nodo di indirizzo 44.

~~~

   6) Stk [  42: NAp   24   41 (NNum 4)
             44: NAp   42   43 (NNum 3)
            ]
~~~
Viene applicato apStep al nodo di indirizzo 42.
~~~      

   7) Stk [  24: NPrim +
             42: NAp   24   41 (NNum 4)
             44: NAp   42   43 (NNum 3)
            ]
~~~
Qui viene applicato primStep al nodo di indirizzo 24 che termina la valutazione della definizione *4 + 3*, l'indirizzo radice (44) viene fatto puntare al risultato dell'operazione
~~~

   8) Stk [  44: NNum 7
            ]
~~~
Ora che la valutazione degli argomenti è completa lo stack precedente viene estratto dal dump e si procede alla valutazione
~~~
      
   9) Stk [  46: NAp   24   45 (NNum 4)
              1: NAp   46   44 (NNum 7)
            ]
      
  10)   Stk [  24: NPrim +
             46: NAp   24   45 (NNum 4)
              1: NAp   46   44 (NNum 7)
            ]
      
  11)   Stk [   1: NNum 11
            ]
  ~~~

Ora riportiamo e discutiamo l'esecuzione del codice: *main = letrec f = x + 3; x = 4 in f*, questo programma necessita l'utilizzo di letrec in quanto la definizione di f dipende dalla definizione di x.

~~~
   1) Stk [   1: NSupercomb main
            ]
  ~~~
  Viene istanziato il corpo del main.
  ~~~
   
   2) Stk [   1: NInd   43
            ]
      
   3) Stk [  43: NAp   41   42 (NNum 3)
            ]
      
   4) Stk [  41: NAp   24   44 (NNum 4)
             43: NAp   41   42 (NNum 3)
            ]
      
   5) Stk [  24: NPrim +
             41: NAp   24   44 (NNum 4)
             43: NAp   41   42 (NNum 3)
            ]
      
   6) Stk [  43: NNum 7
            ]
~~~

Il caso ricorsivo è più complesso in quanto tutti i bindings nome-indirizzo devono essere prodotti prima di istanziare le definizioni, per fare ciò abbiamo sfruttato il fatto che haskell è lazy e quindi non valuta le espressioni a meno che non sia strettamente necessario. In particolare in *istantiateAndUpdate* nel caso ricorsivo di Elet istanzia la parte destra delle definizioni utilizzando i bindings (env1) che in pratica devono ancora essere creati, questo è possibile perchè nella chiamata di *instantiateDef* la funzione instantiate non viene immediatamente chiamata ma viene momentaneamente ritornata la tupla (heap',(name,addr)) dove solamente name è effettivamente valutata. Questo procedimento è eseguito nel primo passo di esecuzione dove il supercombinator main viene istanziato utilizzando *scStep*.

Andiamo ora a vedere un esempio di letrec dove le definizioni formano un ciclo in quanto sono dipendenti tra loro, eseguendo l'esempio:
*pair x y f = f x y ; fs p = p K ; sn p = p K1 ; f x y = letrec a = pair x b ; b = pair y a in fs (sn a) ; main = f 3 4*

~~~
   1) Stk [   5: NSupercomb main
            ]
~~~
Sulla cima dello stack troviamo un super combinator quindi viene applicato *scStep*.
~~~
        
   2) Stk [   5: NAp   46   47 (NNum 4)
            ]
      
   3) Stk [  46: NAp    4   45 (NNum 3)
              5: NAp   46   47 (NNum 4)
            ]
      
   4) Stk [   4: NSupercomb f
             46: NAp    4   45 (NNum 3)
              5: NAp   46   47 (NNum 4)
            ]
~~~
Dopo qualche applicazione di *apStep* troviamo il supercombinator f a cui applichiamo *scStep* che istanzierà il suo body, si tratta di un letrc quindi verranno istanziate le sue definizioni e il suo corpo.
In questo caso le definzioni fanno riferimento l'una all'altra ma non è un problema perchè la cosa è stata gestita come descritto in precedenza. 
~~~
      
   1) Stk [   5: NAp    2   52 (NAp 3 49)
            ]
      
   2) Stk [   2: NSupercomb fs
              5: NAp    2   52 (NAp 3 49)
            ]
~~~
Viene applicato al supercombinator fs la funzion *scStep*, fs si aspetta un argomento il quale nodo (52) e crea l'applicazione (p K)
~~~    
   3) Stk [   5: NAp   52    7 (NSupercomb K)
            ]

   4) Stk [  52: NAp    3   49 (NAp 48 51)
              5: NAp   52    7 (NSupercomb K)
            ]
      
   5) Stk [   3: NSupercomb sn
             52: NAp    3   49 (NAp 48 51)
              5: NAp   52    7 (NSupercomb K)
            ]
~~~
Dopo un certo numero di applicazioni, troviamo il supercombinator sn a cui applichiamo *scStep*, questo aspetta un input che sarà il nodo (49) e crea l'applicazione (p K1) che è innestata in questo modo: (p K1) K, nei prossimi step "aprirà" il costrutto p.
~~~
  1)  Stk [  52: NAp   49    8 (NSupercomb K1)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  2)  Stk [  49: NAp   48   51 (NAp 50 49)
             52: NAp   49    8 (NSupercomb K1)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  3)  Stk [  48: NAp    1   45 (NNum 3)
             49: NAp   48   51 (NAp 50 49)
             52: NAp   49    8 (NSupercomb K1)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  4)  Stk [   1: NSupercomb pair
             48: NAp    1   45 (NNum 3)
             49: NAp   48   51 (NAp 50 49)
             52: NAp   49    8 (NSupercomb K1)
              5: NAp   52    7 (NSupercomb K)
            ]
~~~
Dopo diverse chiamate di *apStep*, abbiamo raggiunto il costrutto pair che prenderà tre argomenti, x y f, rispettivamente i nodi (45) (51) (8), si applica *scStep*, il quale istanzia il body andando a creare l'applicazione (f x) y
~~~
  5)  Stk [  52: NAp   53   51 (NAp 50 49)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  6)  Stk [  53: NAp    8   45 (NNum 3)
             52: NAp   53   51 (NAp 50 49)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  7)  Stk [   8: NSupercomb K1
             53: NAp    8   45 (NNum 3)
             52: NAp   53   51 (NAp 50 49)
              5: NAp   52    7 (NSupercomb K)
            ]
~~~
Dopo diverse chiamate ad *apStep*, troviamo il supercominator K1, che si aspetta 2 parametri, *scStep* ne istanzia il body andando a sostituirgli il secondo parametro, ossia il nodo 51 con una indirezion, questo perché .
~~~
  8)  Stk [  52: NInd   51
              5: NAp   52    7 (NSupercomb K)
            ]
      
  9)  Stk [  51: NAp   50   49 (NAp 48 51)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  10) Stk [  50: NAp    1   47 (NNum 4)
             51: NAp   50   49 (NAp 48 51)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  11) Stk [   1: NSupercomb pair
             50: NAp    1   47 (NNum 4)
             51: NAp   50   49 (NAp 48 51)
              5: NAp   52    7 (NSupercomb K)
            ]
      
  12) Stk [   5: NAp   54   49 (NAp 48 51)
            ]
      
  13) Stk [  54: NAp    7   47 (NNum 4)
              5: NAp   54   49 (NAp 48 51)
            ]
      
  14) Stk [   7: NSupercomb K
             54: NAp    7   47 (NNum 4)
              5: NAp   54   49 (NAp 48 51)
            ]
      
  15) Stk [   5: NInd   47
            ]
      
  16) Stk [  47: NNum 4
            ]
~~~
### Case
Questa implementazione non gestisce le Case expressions, dunque per gestire gli oggetti vengono utilizzati i costrutti if casePair e caseList, oggetti più generici invece non possono esssere gestiti.
### If

~~~
   1) Stk [   1: NSupercomb main
            ]
      
   2) Stk [   1: NAp   47   48 (NNum 6)
            ]
      
   3) Stk [  47: NAp   45   46 (NNum 5)
              1: NAp   47   48 (NNum 6)
            ]
      
   4) Stk [  45: NAp   28   44 (NAp 42 43)
             47: NAp   45   46 (NNum 5)
              1: NAp   47   48 (NNum 6)
            ]
      
   5) Stk [  28: NPrim if
             45: NAp   28   44 (NAp 42 43)
             47: NAp   45   46 (NNum 5)
              1: NAp   47   48 (NNum 6)
            ]
~~~
Possiamo vedere che sulla cima dello stack ci sia l'indirizzo della primitiva If, quindi viene chiamata la funzione *primIf*, questa si aspetta tre argomenti, come quelli presenti nello stack: 44, 46, 48, che sono rispettivamente la condizione dell'if, il ramo del then e quello dell'else. La funzione *primIf* vedendo che la condizione (44) non è stata valutata, sposta lo stack corrente nel dump e inserisce nello stack vuoto l'indirizzo della condizione da valutare.
~~~
    
   6) Stk [  44: NAp   42   43 (NNum 2)
            ]
      
   7) Stk [  42: NAp   29   41 (NNum 1)
             44: NAp   42   43 (NNum 2)
            ]
      
   8) Stk [  29: NPrim >
             42: NAp   29   41 (NNum 1)
             44: NAp   42   43 (NNum 2)
            ]
~~~
Possiamo vedere che sulla cima dello stack ci sia l'indirizzo della primitiva >, quindi viene chiamata la funzione *primComp*, la quale si aspetta due argomenti, in questo caso sono già entrambi valutati, quindi esegue l'operazione 1 > 2 ritornando NData 1 0, ossia falso.
~~~

   9) Stk [  44: NData 1 0
            ]
      
  10) Stk [  28: NPrim if
             45: NAp   28   44 (NData 1 0)
             47: NAp   45   46 (NNum 5)
              1: NAp   47   48 (NNum 6)
            ]
~~~
Ora che la condizione è stata valutata si può eseguire l'if e dato che la condizione è falsa si tolgono gli indirizzi dell'if e si inserisce quello del ramo dell'else, ossia il numero 6.
~~~
      
  11) Stk [  1: NNum 6
            ]
~~~

## Strutture dati
Le strutture dati vengono costruite utilizzando Pack{t,a} dove t è la tag mentre a è il numero di argomenti, questo costrutto è stato rappresentato con la primitiva *PrimConstr t a* quindi possiamo istanziare una espressione EConstr nell'heap come *NPrim "Pack" (PrimConstr t a)*. Abbiamo quindi aggiunto un caso a *primStep* per poter gestire la nuova primitiva che utilzza la funzione ausiliaria *primConstr* che si occupa di controllare se al costruttore vengono dati abbastanza argomenti, se questo è vero allora viene istanziato un oggetto nell'heap che sarà rappresentato dal tipo di nodo NDdata che conterrà il tag e l'indirizzo dei vari argomenti.

Per rappresentare True e False abbiamo utilizzato rispettivamente Pack{2,0} e Pack{1,0} inserendo le definizioni in *extraPreludeDefs*.

### Coppie

Per gestire le coppie utilizziamo la costruzione Pack{1,2}, questo porterà alla costruzione di un NData che contiene il tag 1 e 2 indirizzi, per poter estrarre un nodo dalla coppia utilizziamo *casePair*, costruita come una primitiva questa funzione prende una coppia e vi applica una funzione ausiliaria. per ottenere il primo elemento della coppia basterà scrivere casepair p K dove p è una coppia (K è una funziona che dati due argomenti restituisce solo il primo).

Di seguito mostriamo l'esecuzione del programma "main = let p = Pack{1,2} 4 5 in casePair p K"
~~~

   1) Stk [   1: NSupercomb main
            ]
~~~
Viene istanziato il body del supercombinator main con il comando scStep, viene quindi istanziata la definizione Pack{1,2} 4 5 del let e viene sostituito il nodo radice con il nodo corrispondente al body del let.
~~~
      
   2) Stk [   1: NAp   46    3 (NSupercomb K)
            ]
~~~
Viene applicato apStep
~~~
   3) Stk [  46: NAp   35   45 (NAp 43 44)
              1: NAp   46    3 (NSupercomb K)
            ]
~~~
Viene applicato nuovamente apStep
~~~
      
   4) Stk [  35: NPrim casePair
             46: NAp   35   45 (NAp 43 44)
              1: NAp   46    3 (NSupercomb K)
            ]
~~~
Visto che in cima allo stack si trova una primitiva viene chiamato *primStep* che utilizzerà *primCasePair* la quale proverà ad applicare la funzione in (3) a (45) ma in questo caso l'espressione (45) non è ancora stata valutata quindi lo stack viene spostato nel dump e si mette nello stack l'esperssione da valutare.
~~~
      
   5) Stk [  45: NAp   43   44 (NNum 5)
            ]

   6) Stk [  43: NAp   41   42 (NNum 4)
             45: NAp   43   44 (NNum 5)
            ]
      
   7) Stk [  41: NPrim Pack
             43: NAp   41   42 (NNum 4)
             45: NAp   43   44 (NNum 5)
            ]
~~~
A questo punto troviamo la primitiva pack in cima allo stack quindi viene chiamata la funzione *primConstr* che prenderà attraverso *getargs* gli argomenti a indirizzo (42) e (44) e costruirà un NData con tag 1 e contenente una lista di 2 argomenti
~~~

   8) Stk [  45: NData 1 2
            ]
~~~
Visto che in cima allo stack c'è un NData l'espressione è stata completamente valutata quindi si estrae il vecchio stack dal dump e si continua la valutazione 
~~~
      
   9) Stk [  46: NAp   35   45 (NData 1 2)
              1: NAp   46    3 (NSupercomb K)
            ]
      
  10) Stk [  35: NPrim casePair
             46: NAp   35   45 (NData 1 2)
              1: NAp   46    3 (NSupercomb K)
            ]

~~~
Questa volta *casePair* troverà la coppia valutata quindi potrà applicare la funzione K a (4,5)
~~~      

  11) Stk [   1: NAp   47   44 (NNum 5)
            ]
      
  12) Stk [  47: NAp    3   42 (NNum 4)
              1: NAp   47   44 (NNum 5)
            ]
      
  13) Stk [   3: NSupercomb K
             47: NAp    3   42 (NNum 4)
              1: NAp   47   44 (NNum 5)
            ]
~~~
K è un supercombinator, in questo caso è una funzione definita nel Prelude, quindi viene utilizzata *scStep*
~~~
      
  14) Stk [   1: NInd   42
            ]
      
  15) Stk [  42: NNum 4
            ]

~~~
Il risultato finale è quindi il numero 4 estratto dalla coppia (4,5)

### Liste

Utilizziamo una definizione ricorsiva delle liste:

~~~
list * ::= Nil | Cons * (list *)
~~~

*Nil* corrisponderà a Pack{1,0} mentre *Cons* corrisponderà a Pack{2,2}, ora per gestire questo tipo di lista implementiamo una nuova primitiva ovvero *caseList* che ha il compito di ritornare un caso base quando ha in input una lista vuota (Nil) altrimenti applicare una funzione ausiliaria sugli elementi della lista. anche *caseList* è stata implementata come una primitiva.

~~~
caseList Pack{1,0} cn cc = cn
caseList (Pack{2,2} x xs) cn cc = cc x xs
~~~

Per poter ottenere una lista come output di un programma è stato necessario fare delle modifiche al codice: per prima cosa è stato aggiunto output a TiState, questo nuovo campo, una volta eseguito tutto il programma, conterrà la lista risultante.
Per ottenere questa stampa sono state implementate le primitiva stop, che si occupa di mettere TiState in una configurazione finale quando la lista è stata stampata, e print che si occupa di mettere il valore corrente nell'output.
Queste due funzioni vengono utilizzate in questo modo per poter stampare l'intera lista risultante:

~~~
printList xs = caseList xs stop printCons
printCons h t = print h (printList t)
~~~

Per fare si che il risultato possa essere stampato è il compilatore deve essere modificato in modo che lo stack iniziale non contenga più solo il main ma l'applicazione (printList main).

~~~
   1) Stk [  40: NAp   21    1 (NSupercomb main)
            ]
~~~
Notiamo che ora lo stack iniziale contiene una applicazione che corrisponde a *printList main* dunque per prima cosa verrà chiamata *apStep*
~~~
      
   2) Stk [  21: NSupercomb printList
             40: NAp   21    1 (NSupercomb main)
            ]
~~~
ora in cima allo stack troviamo il supercombinator definito in extraPreludeDefs *printList* che viene istanziato chiamando *scStep*
~~~
   3) Stk [  40: NAp   42   22 (NSupercomb printCons) 
            ]
~~~
Nello stack viene dunque inserita la definizione di *printList* che viene 'aperta' utilizzando più volte *apStep*
~~~
   4) Stk [  42: NAp   41   39 (NPrim stop)
             40: NAp   42   22 (NSupercomb printCons)
            ]
      
   5) Stk [  41: NAp   37    1 (NSupercomb main)
             42: NAp   41   39 (NPrim stop)
             40: NAp   42   22 (NSupercomb printCons)
            ]
      
   6) Stk [  37: NPrim caseList
             41: NAp   37    1 (NSupercomb main)
             42: NAp   41   39 (NPrim stop)
             40: NAp   42   22 (NSupercomb printCons)
            ]
~~~
Dopo aver 'aperto' tutte quante le applicazioni si arriva al nodo *NPrim caselist*, il quale necessita di 3 argomenti: lista (main), caso base (stop), caso ricorsivo (printCons), dato che il main non è ancora stato valutato si archivia lo stack nel dump e si pone nel nuovo stack l'indirizzo corrispondente al main che verrà valutato.
~~~
   7) Stk [   1: NSupercomb main
            ]
      
   8) Stk [   1: NAp   45   46 (NPrim Pack)
            ]
      
   9) Stk [  45: NAp   43   44 (NNum 5)
              1: NAp   45   46 (NPrim Pack)
            ]
      
  10) Stk [  43: NPrim Pack
             45: NAp   43   44 (NNum 5)
              1: NAp   45   46 (NPrim Pack)
            ]
      
  11) Stk [   1: NData 2 2
            ]
~~~
Ora che il main è stato valutato, possiamo riprendere dal dump lo stack precedente e ripartire dove ci eravamo interrotti.
~~~
  12) Stk [  41: NAp   37    1 (NData 2 2)
             42: NAp   41   39 (NPrim stop)
             40: NAp   42   22 (NSupercomb printCons)
            ]
      
  13) Stk [  37: NPrim caseList
             41: NAp   37    1 (NData 2 2)
             42: NAp   41   39 (NPrim stop)
             40: NAp   42   22 (NSupercomb printCons)
            ]
~~~
Ora ci troviamo nella stessa situazione incontrata nello step 6, solo che ora il nodo 1 è valutato quindi si può applicare la regola di caseList chiamando *primCaseList* che visto che la lista non è vuota applica *printCons* alla lista creando l'applizazione e mettendola nello stack
~~~
  14) Stk [  40: NAp   47   46 (NPrim Pack)
            ]
~~~
ecco l'applicazione che viene 'aperta' usando apStep 
~~~  
  15) Stk [  47: NAp   22   44 (NNum 5)
             40: NAp   47   46 (NPrim Pack)
            ]
      
  16) Stk [  22: NSupercomb printCons
             47: NAp   22   44 (NNum 5)
             40: NAp   47   46 (NPrim Pack)
            ]
~~~
ora che l'applicazione è aperta si applica la definizione di *printCons* definita in *extraPreludeDefs* utilizzando *scStep*, quello che farà questa funzione è applicare la primitiva *print* alla testa della lista e applicare *printList* alla coda
~~~
  17) Stk [  40: NAp   48   49 (NAp 21 46)
            ]
      
  18) Stk [  48: NAp   38   44 (NNum 5)
             40: NAp   48   49 (NAp 21 46)
            ]
      
  19) Stk [  38: NPrim print
             48: NAp   38   44 (NNum 5)
             40: NAp   48   49 (NAp 21 46)
            ]
  ~~~
  Ora che l'applicazione di *printCons* è stata 'aperta' applichiamo la primitiva *print* che metterà il numero in testa alla lista nell'output e metterà nello stack l'applicazione di *printList* sul resto della lista
  ~~~   
  20) Stk [  49: NAp   21   46 (NPrim Pack)
            ]
      
  21) Stk [  21: NSupercomb printList
             49: NAp   21   46 (NPrim Pack)
            ]
~~~
Come prima chiamiamo *scStep* per istanziare il body di *printList*  
~~~
  22) Stk [  49: NAp   51   22 (NSupercomb printCons)
            ]
      
  23) Stk [  51: NAp   50   39 (NPrim stop)
             49: NAp   51   22 (NSupercomb printCons)
            ]
      
  24) Stk [  50: NAp   37   46 (NPrim Pack)
             51: NAp   50   39 (NPrim stop)
             49: NAp   51   22 (NSupercomb printCons)
            ]
      
  25) Stk [  37: NPrim caseList
             50: NAp   37   46 (NPrim Pack)
             51: NAp   50   39 (NPrim stop)
             49: NAp   51   22 (NSupercomb printCons)
            ]
   ~~~
   Nuovamente ci troviamo con l'applicazione di *caseList* che però contiene una lista non ancora valutata (46) quindi si procede a mettere lo stack corrente nel dump e a valutare la lista
   ~~~
  26) Stk [  46: NPrim Pack
            ]
      
  27) Stk [  46: NData 1 0
            ]
      
  28) Stk [  50: NAp   37   46 (NData 1 0)
             51: NAp   50   39 (NPrim stop)
             49: NAp   51   22 (NSupercomb printCons)
            ]

  29) Stk [  37: NPrim caseList
             50: NAp   37   46 (NData 1 0)
             51: NAp   50   39 (NPrim stop)
             49: NAp   51   22 (NSupercomb printCons)
            ]
   ~~~
   Ora che la lista è valutata possiamo applicare *caseList*, visto che NData 1 0 corrisponde alla lista vuota viene messa in cima allo stack la primitiva *stop* che svuoterà lo stack mettendo così il programma in uno stato finale
   ~~~  
  30) Stk [  49: NPrim stop
            ]
      
  31) Stk [ ]


Main = 5
~~~
Ecco il risultato dell'esecuzione, viene stampata la lista formata solo dal valore 5.
### Garbage Collector

Il garbage collector ha il compito di eliminare i nodi che non vengono utilizzati ed è rappresentato dalla funzione *gc*, questa viene chiamata da *doAdmin* prima di fare uno step se l'heap contiene un numero di nodi troppo elevato.
In pratica la funzione *gc* grazie ad altre funzioni ausiliare marchia i nodi che sono raggiungibili tramite stack, globals e dump e tutti i nodi raggiungibili da essi utilizzando delle chiamate ricorsive della funzione, una volta fatta questa operazione viene applicata la funzione *hFree* su tutti i nodi non marchiati.

Il problema di questa implementazione è il fatto che se tutti i nodi dello heap sono collegati allora per poter marchiare tutti i nodi occorre avere uno stack grande quanto l'heap, per questo motivo abbiamo sostituito la funzione di marking con una macchina a stati che invece di utilizzare la ricorsione per tornare al nodo precedente usa dei veri e propri puntatori al nodo corrente e al nodo precedente, questa macchina a stati è implementata dalla funzione *markStateMachine* e compie una azione specifica a seconda del nodo corrente e del nodo precedente. 
Questa implementazione ha reso necessario aggiungere il campo Visits al nodo NMarked che ricorda il numero di visite al nodo e che viene messo a Done quando tutti i nodi raggiungibili da esso sono stati marchiati.