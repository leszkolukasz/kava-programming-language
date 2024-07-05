## Wstęp

Język wzorowany jest na Kotlinie z zapożyczeniami z JavaScript.

Składa się z dwóch części: interpreter i type checker. Obydwa korzystają z monad
transformerów `ReaderT`, `StateT` i `ExceptT`. W gramatyce jest jeden shift/reduce conflict,
który występuje w typie funkcji, która zwracaja listę np.

```
F(int) -> int[]
```

problem jest taki, że nie wiadomo czy interpretować to jako `(F(int) -> int)[]`
czy jako `F(int) -> (int[])`. Natomiast domyślnie traktowane jest to jako to drugie,
czyli tak jak początkowo chciałem. Jest to zatem nieszkodliwy konflikt.

## Syntax

### Typy

Język wspiera następujące typy: int, string, bool, listy określonego typu (w tym listy funkcji), funkcje.

Przykłady typów:

```
int
string
int[] // lista intów
F(int, string) -> bool[] // funkcja dwóch parametrów int i string. Zwraca listę booli
```

### Literały

Standardowe:

```
"string";
10;
List.of(1, 2, 3);

// Dla funkcji:
(a: int, b: bool[]): int -> { return a; };
```

### Zmienne

Zmienne deklaruje się w następujący sposób:

```
let x: <TYPE> = ...
const y: <TYPE> = ...

let z: <TYPE>;
```

Niezainicjalizowana zmienna ma wartość zależną od typu:

```
string -> ""
int -> 0
bool -> false
lista -> []
funkcja -> Exception. Musi być zainicjalizowana
```

Zaimplementowany jest prosty type inference. Jeśli typ zmiennej można wywnioskować
z kontekstu, to nie trzeba go podawać:

```
// x jest typu int
auto x = 10;

// y jest typu (int) -> int
auto y = (a: int): int -> { return x; };
```

### Porównania

Porównania są strukturalne:

```
List.of(1, 2) == List.of(1, 2); // true
List.of(List.of(1, 2), List.of(3, 4)) == List.of(List.of(1, 2), List.of(3, 4)) // true

let x = "x";
let y = "x";

x == y // true
true == false // false
```

Funkcji nie można porównywać. Porównania "<", ">", "<=", ">=" wspierane są
tylko dla intów i stringów. W przypadku stringów porównanie jest leksykograficzne.

### Arytmetyka

Standardowa.

Dla intów: dodawanie, odejmownie, mnozenie, dzielenie, modulo.
Dla list: dodawanie (konkatencja)
Dla stringów: dodawanie (konkatenacja)

Wspierany syntactic sugar:

```
+=
-=
*=
/=
%=
```

### Operacje logiczne

```
&&
||
!
```

### Wbudowane funkcje

```
print(x) // wypisuje x, x może być dowolnego typu
len(list) // zwraca długość listy
```

### Instrukcje sterujące

while, if (w wersji z else if oraz else):

```
while (<BExp>) {

}

if (<BExp>) {

} elif (<BExp>) {

} else {

}

for (const <IDEN> of <LIST>) {
    // iteruje zmienną (która jest const) po elementach listy
}

```

### Listy

Indeskowanie (w tym ujemnymi wartościami). Generowanie przedziałów.

```
len(List.of()) == 0 // true
len(List.of("a")) == 1 // true

List.of("a", "b")[0] == "a" // true
List.of("a", "b")[-1] = "b" // true

x..y == List.of(x, x+1, ..., y-1, y) // gdy x < y
x..y == List.of(x, x-1, ..., y+1, y) // gdy x > y
// x, y muszą być intami, mogą być ujemne
```

### Funkcje

Statyczne wiązanie, rekurencja, funkcje zagnieżdzone, przekazywanie parametrów
przez wartość, przesłanianie zmiennych, zmienne globalne.

Funkcje muszą być zadeklarowane przed użyciem.

```
const global: int = 0;

fun foo(x: int, const y: string): string {
    fun bar(y: string): void {
        y += "b";
        print(y);
    }

    bar(y);
    print(y); // niezmienione y
    return y;
}
```

### Funkcje anonimowe + closure

```
fun foo(): F(int) -> int {
    let y: int = 0;
    const f: F(int) -> int = (x: int): int -> { y++; return x+y; }
    return f;
}
```

## Tabelka cech

```
  Na 15 punktów
  01 (trzy typy) x
  02 (literały, arytmetyka, porównania) x
  03 (zmienne, przypisanie) x
  04 (print) x
  05 (while, if) x
  06 (funkcje lub procedury, rekurencja) x
  07 (przez zmienną / przez wartość / in/out) (przez wartość)
  08 (zmienne read-only i pętla for) x
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie) x
  10 (obsługa błędów wykonania) x
  11 (funkcje zwracające wartość) x
  Na 30 punktów
  12 (4) (statyczne typowanie) x
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem) x
  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) (listy)
  15 (2) (krotki z przypisaniem)
  16 (1) (break, continue)
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia) x
  18 (3) (generatory)

Razem: 30

```
