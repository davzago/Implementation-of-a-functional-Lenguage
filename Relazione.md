# Template Instantiation

Il progetto che abbiamo realizzato è basato su implementing functional lenguages, in particolare ci siamo focalizzati su un interprete basato su template instantiation. L'obbiettivo della nostra implementazione è quello di eseguire un programma scritto in Core Lenguage dato in input, in un linguaggio funzionale ogni programma è formato da una o più espressioni le quali devono essere valutate. Ciascuna epressione è rappresentata da un grafo e per valutarle bisongo fare una serie di riduzioni fino a raggiungere una forma normale.

## Passaggi di Esecuzione

Di seguito descriviamo i passaggi che portano alla effettiva esecuzione del programma.

### Parser

Il parser è il componente che si occupa di elaborare il programma testuale preso come input e di restituire delle espressioni, questo passaggio è fondamentale pre controllare che il programma non presenti errori sintattici

### Compilatore

Il compilatore prende in input la lista di espressioni fornite dal parser e si occupa di creare lo stato iniziale del programma, lo stato consiste in:

- **Stack:** Una pila di indirizzi, ognuno dei quali identifica un nodo nell'*heap* 
- **Dump:** Una pila di stack che ha come obbiettivo quello di ricordare quali sono le valutazioni precedenti a cui ritornare quando la valutazione corrente è terminata
- **Heap:** Una collezione di coppie chiave valore dove la chiave è un indirizzo e il valore è un nodo che identifica la tipologia di una espressione 
- **Globals:** Una collezione che tiene che collega il nome di una definizione con il suo indirizzo nello heap

lo stato iniziale è formato da uno stack che contiene l'indirizzo del main (definizione obbligatoria per ogni programma), dump vuoto, heap contenente tutte le definizioni iniziali ovvero le definizioni del programma e le definizioni del prelude mentre globals contiene il nome di ogni definizione con associato il suo indirizzo nello heap.


### Valutatore

Per valutare il programma abbiamo utilizzato uno state transition system, questo significa che a meno di errori, partendo da uno stato iniziale il valutatore compie dei passi fino ad arrivare ad uno stato finale. Per ogni stato il sitema compie un passo dettato da delle regole di transizione.

## Implementazione

Nella pratica il valutatore controlla a che tipo di nodo corrisponde l'indirizzo in cima allo stack e applica la regola di transizione adeguata, i possobili tipi di nodo sono:

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

Di seguito mostriamo un esempio di esecuzione del programma main = S K K 3 e discutiamo la sua esecuzione. 

~~~ 
1) Stk [   1: NSupercomb main
            ]
   1) Stk [  11: NAp    9   10 (NNum 3)
            ]
   2) Stk [   9: NAp    8    3 (NSupercomb k)
             11: NAp    9   10 (NNum 3)
            ]
   3) Stk [   8: NAp    5    3 (NSupercomb k)
              9: NAp    8    3 (NSupercomb k)
             11: NAp    9   10 (NNum 3)
            ]
   4) Stk [   5: NSupercomb s
              8: NAp    5    3 (NSupercomb k)
              9: NAp    8    3 (NSupercomb k)
             11: NAp    9   10 (NNum 3)
            ]
   5) Stk [  14: NAp   12   13 (NAp 3 10)
            ]
   6) Stk [  12: NAp    3   10 (NNum 3)
             14: NAp   12   13 (NAp 3 10)
            ]
   7) Stk [   3: NSupercomb k
             12: NAp    3   10 (NNum 3)
             14: NAp   12   13 (NAp 3 10)
            ]
   8) Stk [  10: NNum 3]
Total number of steps = 8
~~~

Inizialmente lo stack contiene solamente l'indirizzo del main che è un supercombinator dunque viene chiamata la funzione *scStep*, visto che il main non ha alcun argomento   