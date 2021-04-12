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

La funzione *compile* per prima cosa mette in cima allo stack l'indirizzo del main, su questo stato quindi viene chiamata la funzione *eval* che procede a chiamare *step* finchè non si ottiene uno stato finale, ogni chiamata di *step* chiamerà la funzione corretta, in questo caso visto che il main è un supercombinator viene chiamata la funzione *scStep* che instanzia nello heap il corpo del supercombinator e imposta il suo indirizzo come radice che deve essere aggiornata una volta valutata l'espressione.
Una volta istanziato il supercombinator si nota che nello stack è presente una applicazione dunque viene chiamata la funzione *apStep* che descatola il suo primo argomento, questo viene fatto da 2) a 5) dove si trova il supercombinator S in cima allo stack, in questo stato viene dunque chiamata nuovamente *scStep* che procede ad istanziare il corpo di S definito in questo modo nelle definizioni del prelude:

~~~
("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
~~~

In pratica la funzione S prende due funzioni ed un argomento e crea questa applicazione : f x (g x), visto che si tratta di un' applicazione viene usata nuovamente *apStep* che cerca l'applicazione outermost fino a quando trova K che è un super combinator definito in questo modo:

~~~
("K", ["x","y"], EVar "x")
~~~

Dunque il body di questo supercombinator è semplicemente una Evar x che per mano di *instantiateAndUpdate* viene collegata all'argomento corretto da *getargs* ovvero NNum 3, la radice viene quindi aggiornata a questo valore.
Nella successiva chiamata di step *TiFinal* ci dice che ci troviamo in uno stato finale in quanto nello stack c'è un solo indirizzo di un numero e il dump è vuoto qundi la valutazione termina.  

- Let letrec
- mkPair
- case expressions
- liste e perchè fatte così
- gc