## Wstęp

Język wzorowany będzie na Kotlinie z zapożyczeniami z JavaScript.

## Syntax

### Typy

Język będzie wspierał następujące typy: int, string, bool, listy określonego typu (w tym listy funkcji), funkcje.

Przykłady typów:

```
int
string
int[] // lista intów
(int, string) -> bool[] // funkcja dwóch parametrów int i string. Zwraca listę booli
```

### Literały

Standardowe:

```
"string";
10;
[1, 2, 3]; // nie wiem czy literały listowe będą wspierane

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

Jeśli się uda: prosty Type Inference

```
// x jest typu int
auto x = 10;

// y jest typu (int) -> int
auto y = (a: int): int -> { return x; };
```

### Porównania

Porównania będą strukturalne:

```
[1, 2] == [1, 2]; // true
[[1, 2], [3, 4]] == [[1, 2], [3, 4]] // true

let x = "x";
let y = "x;

x == y // true
```

Porównanie funkcji zwraca false.

### Arytmetyka

Standardowa.

Dla intów: dodawanie, odejmownie, mnozenie, dzielenie, modulo. \
Dla list: dodawanie (konkatencja) \
Dla stringów: dodawanie (konkatenacja)

Wspierany syntactic sugar:

```
++ (post i pre)
-- (post i pre)
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

|| i && są leniwe.

### Wbudowane funkcje

```
print(x) // wypisuje x, x może być typu: int, string, list, bool
typeof(x) // zwraca typ zmiennej jako string
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

for (const <IDEN> in <LIST>) {
    // iteruje zmienną (która jest const) po elementach listy
}

```

### Listy

Indeskowanie. Slicing (jak w numpy) i generowanie przedziałów (jeśli starczy czasu).

```
len([]) == 0 // true
len(["a"]) == 1 // true

["a", "b"][0] == "a" // true
["a", "b", "c"][0:2] == ["a", "b"]
["a", "b"][-1] = "b"

x..y == [x, x+1, ..., y-1] // x, y muszą być typu int
```

### Funkcje

Statyczne wiązanie, rekurencja, funkcje zagnieżdzone, przekazywanie parametrów przez wartość, przesłanianie zmiennych, zmienne globalne.

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
fun foo(): (int) -> int {
    let y: int = 0;
    const f: (int) -> int = (x: int): int -> { y++; return x+y; }
    return f;
}
```

## Tabelka cech (na 30pkt)

```
1. Co najmniej trzy typy wartości: int, bool i string (to znaczy if 2+2 then _ parsuje się, ale wyrażenie ma niepoprawny typ).
2. Literały, arytmetyka, porównania.
3. Zmienne, operacja przypisania
4. Jawne wypisywanie wartości na wyjście (instrukcja lub wbudowana procedura print).
5. while, if (z else i bez, może być też składnia if _ elif _ else _ endif).
6. Funkcje lub procedury (bez zagnieżdżania), rekurencja.
8. zmienne „read-only” i użycie ich np. w implementacji pętli for w stylu Pascala (for i = pocz to kon) wewnątrz pętli nie można zmienić wartości zmiennej sterującej, wartość kon liczona tylko raz - przed wejściem do pętli
9. Przesłanianie identyfikatorów ze statycznym ich wiązaniem (zmienne lokalne i globalne lub zagnieżdżone procedury/funkcje).
10. Obsługa błędów wykonania, np. dzielenie przez zero (może być elegancki komunikat i zatrzymanie interpretera).
11. Funkcje przyjmujące i zwracające wartość dowolnych obsługiwanych typów (tzn. nie tylko procedury; za to mogą być tylko funkcje – jak w języku C)
12. Statyczne typowanie (tj. zawsze terminująca faza kontroli typów przed rozpoczęciem wykonania programu) – 4pkt,
13. Dowolnie zagnieżdżone definicje funkcji / procedur z zachowaniem poprawności statycznego wiązania identyfikatorów (jak w Pascalu) – 2 pkt
14. Rekordy albo tablice indeksowane int albo coś à la listy – 1pk
17. Funkcje jako parametry, zwracanie funkcji w wyniku, domknięcia à la JavaScript. Funkcje anonimowe – 4pkt,
```

## EBNF

```
(** Types **)
INT_TYPE = "int";
STRING_TYPE = "string";
BOOL_TYPE = "bool";
LIST_TYPE = TYPE, "[]";
FUNCTION_TYPE = "(", { TYPE, { "," , TYPE } } ")", "->", TYPE;
TYPE = INT_TYPE | STRING_TYPE | BOOL_TYPE | FUNCTION_TYPE |  "(" , TYPE , ")";

(** Expressions **)
LITERAL = INT | STRING | BOOL | LIST | ANON_FUNCTION;
EXP = LITERAL | IDEN | IDEN , "(", { EXP, { ",", EXP } }, ")" | EXP, OP, EXP | "(", EXP, ")" | PRE_OP, EXP | EXP, POST_OP | EXP, LIST_OP | LIST_RANGE;
OP = "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||";
PRE_OP = "++" | "--" | "!" | "-";
POST_OP = "++" | "--";
ANON_FUNCTION = "(", { IDEN, ":", TYPE, { ",", IDEN, ":", TYPE } }, ")", ":", TYPE, "->", BLOCK;
LIST_OP = "[", INT, "]" | "[", INT, ":", INT, "]";
LIST_RANGE = INT, "..", INT;

(** Statements **)
STMT = ";" | BLOCK | DECL | ASSIGN | IF | WHILE | FOR | RETURN | FUN | EXP , ";";
BLOCK = "{", { STMT }, "}";
DECL = ("let" | "const"), IDEN, ":", TYPE, ["=", EXP], ";";
ASSIGN = IDEN, "=", EXP, ";";
IF = "if", "(", EXP, ")", BLOCK, {"elif", "(", EXP, ")", BLOCK}, ["else", BLOCK];
WHILE = "while", "(", EXP, ")", BLOCK;
FOR = "for", "(", "const", IDEN, "in", EXP, ")", BLOCK;
RETURN = "return", [EXP], ";";
FUN = "fun", IDEN, "(", { IDEN, ":", TYPE, { ",", IDEN, ":", TYPE } }, ")", ":", TYPE, BLOCK;

(** Program **)

PROGRAM = { STMT };

(** Comments **)
COMMENT = "//", { CHAR };

```
