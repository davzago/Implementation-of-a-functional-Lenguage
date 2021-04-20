# Template Instantiation

Il progetto che abbiamo realizzato è basato sul secondo capitolo del libro "implementing functional lenguages", in particolare ci siamo focalizzati su un interprete basato su template instantiation. L'obbiettivo della nostra implementazione è quello di eseguire un programma scritto in Core Language, in un linguaggio funzionale ogni programma è formato da una o più espressioni le quali devono essere valutate. Ciascuna espressione è rappresentata da un grafo e per valutarle bisogno fare una serie di riduzioni fino a raggiungere una forma normale.

## Passaggi di Esecuzione

Di seguito descriviamo i passaggi che portano alla effettiva esecuzione del programma.

### Parser

Il parser è il componente che si occupa di elaborare il programma testuale preso come input e di restituire delle espressioni, questo passaggio è fondamentale per controllare che il programma non presenti errori sintattici

### Compilatore

Il compilatore prende in input la lista di espressioni fornite dal parser e si occupa di creare lo stato iniziale del programma, lo stato consiste in:

- **Stack:** Una pila di indirizzi, ognuno dei quali identifica un nodo nell'*heap* 
- **Dump:** Una pila di stack che ha come obbiettivo quello di ricordare quali sono le valutazioni precedenti a cui ritornare quando la valutazione corrente è terminata
- **Heap:** Una collezione di coppie chiave valore dove la chiave è un indirizzo e il valore è un nodo che identifica la tipologia di una espressione 
- **Globals:** Una collezione che tiene che collega il nome di una definizione con il suo indirizzo nello heap

lo stato iniziale è formato da uno stack che contiene l'indirizzo del main (definizione obbligatoria per ogni programma), dump vuoto, heap contenente tutte le definizioni iniziali ovvero le definizioni del programma e le definizioni del prelude mentre globals contiene il nome di ogni definizione con associato il suo indirizzo nello heap.


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

Di seguito mostriamo un esempio di esecuzione del programma "main = S K K 3" e discutiamo la sua esecuzione. 

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

La funzione *compile* per prima cosa mette in cima allo stack l'indirizzo del main, su questo stato quindi viene chiamata la funzione *eval* che procede a chiamare *step* finché non si ottiene uno stato finale, ogni chiamata di *step* chiamerà la funzione corretta, in questo caso visto che il main è un supercombinator viene chiamata la funzione *scStep* che istanzia nello heap il corpo del supercombinator e imposta il suo indirizzo come radice che deve essere aggiornata una volta valutata l'espressione.
Una volta istanziato il supercombinator si nota che nello stack è presente una applicazione dunque viene chiamata la funzione *apStep* che descatola il suo primo argomento, questo viene fatto da 2) a 5) dove si trova il supercombinator S in cima allo stack, in questo stato viene dunque chiamata nuovamente *scStep* che procede ad istanziare il corpo di S definito in questo modo nelle definizioni del prelude:

~~~
("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
~~~

In pratica la funzione S prende due funzioni ed un argomento e crea questa applicazione : f x (g x), visto che si tratta di un' applicazione viene usata nuovamente *apStep* che cerca l'applicazione outermost fino a quando trova K che è un super combinator definito in questo modo:

~~~
("K", ["x","y"], EVar "x")
~~~

Dunque il body di questo supercombinator è semplicemente una Evar x che per mano di *instantiateAndUpdate* viene collegata all'argomento corretto da *getargs* ovvero NNum 3, la radice viene quindi aggiornata a questo valore.
Nella successiva chiamata di step *TiFinal* ci dice che ci troviamo in uno stato finale in quanto nello stack c'è un solo indirizzo di un numero e il dump è vuoto quindi la valutazione termina.  

Tra le varie istanziazioni di espressioni la più difficoltosa è quella di let, specialmente nel caso ricorsivo infatti quando alcune definizioni formano dei cicli. Il problema è che quando si sta istanziando una definizione potrebbe essere necessaria l'associazione nome indirizzo di altre definizioni non ancora istanziate, per risolvere questo problema abbiamo utilizzato il fatto che haskell è lazy, quindi nel caso ricorsivo quando un si sta allocando una definizione viene passata la lista di associazioni nome indirizzo aumentata con tutte le definizioni anche se effettivamente non è ancora stata creata. In un linguaggio non lazy sarebbe stato necessario fare questa cosa a mano tenendo degli indirizzi da parte per poter allocare la definizione.

### Aritmetica

Per implementare le operazioni aritmetiche utilizziamo le primitive:

~~~ haskell
data Primitive = Neg | Add | Sub | Mul | Div | PrimConstr Tag Arity 
                | If | Greater | GreaterEq | Less | LessEq | Eq | NotEq 
                | PrimCasePair | Abort | PrimCaseList | Stop | Print
~~~

Per prima cosa creiamo una lista di associazioni nome-primitiva, ad esempio '+' corrisponderà alla primitiva Add, in questo modo quando il parser restituirà una EVar '+' potremmo creare il nodo NPrim '+' Add nello heap.
Successivamente per applicare effettivamente queste operazioni viene usata la funzione *primStep* che a seconda della primitiva che viene trovata in cima allo stack decide cosa fare. In generale per le operazioni aritmetiche quello che accade è che se gli argomenti dell'operazione sono valutati si procede con l'operazione altrimenti si procede a valutare gli argomenti mettendo lo stack corrente nel dump e mettendo in cima allo stack l'argomento da valutare.
Le primitive sono state utilizzate anche per implementare l'If, operazioni booleane e per costruire oggetti.

Questa implementazione non gestisce le Case expressions, dunque per gestire gli oggetti vengono utilizzati i costrutti if casePair e caseList, oggetti più generici invece non possono esssere gestiti.

### Coppie

Per gestire le coppie utilizziamo la costruzione Pack{1,2}, questo porterà alla costruzione di un NData che contiene il tag 1 e 2 indirizzi, per poter estrarre un nodo dalla coppia utilizziamo *casePair*, costruita come una primitiva questa funzione prende una coppia e vi applica una funzione ausiliaria. per ottenere il primo elemento della coppia basterà scrivere casepair p K dove p è una coppia. 

### Liste

Utilizziamo una definizione ricorsiva delle liste:

~~~
list * ::= Nil | Cons * (list *)
~~~
*Nil* corrisponderà a Pack{1,0} mentre *Cons* corrisponderà a Pack{2,2}, ora per gestire questo tipo di lista implementiamo una nuova primitiva ovvero *caseList* che ha il compito di ritornare un caso base quando ha in input una lista vuota (Nil) altrimenti applicare una funzione ausiliaria sugli elementi della lista. anche *caseList* è stata implementata come una primitiva.
Per poter ottenere una lista come output di un programma è stato necessario fare delle modifiche al codice: per prima cosa è stato aggiunto output a TiState, questo nuovo campo una volta eseguito tutto il programma conterrà la lista risultante.
Per ottenere questa stampa sono state implementate le primitiva stop, che si occupa di mettere TiState in una configurazione finale quando la lista è stata stampata e print che si occupa di mettere il valore corrente nell'output.
queste due funzioni vengono chiamate in questo modo per poter stampare l'intera lista risultante:

~~~
printList xs = caseList xs stop printCons
printCons h t = print h (printList t)
~~~

Per fare si che il risultato possa essere stampato è il compilatore deve essere modificato in modo che lo stack iniziale non contenga più solo il main ma l'applicazione (printList main).

### Garbage Collector

Il garbage collector ha il compito di eliminare i nodi che non vengono utilizzati ed è rappresentato dalla funzione *gc* che viene chiamata da *doAdmin* prima di fare uno step se l'heap contiene un numero di nodi troppo elevato.
In pratica la funzione *gc* grazie ad altre funzioni ausiliare marchia i nodi che sono raggiungibili tramite stack, globals e dump e tutti i nodi raggiungibili da essi utilizzando delle chiamate ricorsive della funzione , una volta fatta questa operazione viene applicata la funzione *hFree* su tutti i nodi non marchiati.

Il problema di questa implementazione è il fatto che se tutti i nodi dello heap sono collegati allora per poter marchiare tutti i nodi occorre avere uno stack grande quanto l'heap, per questo motivo abbiamo sostituito la funzione di marking con una macchina a stati che invece di utilizzare la ricorsione per tornare al nodo precedente usa dei veri e propri puntatori al nodo corrente e al nodo precedente, questa macchina a stati è implementate dalla funzione *markStateMachine* e compie una azione specifica a seconda del nodo corrente e del nodo precedente. 
Questa implementazione ha reso necessario aggiu	ngere il campo Visits al nodo NMarked che ricorda il numero di visite al nodo e che viene messo a Done quando tutti i nodi raggiungibili da esso sono stati marchiati.