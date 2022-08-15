# Einführung

## Was ist Haskell?

Haskell ist eine rein funktionale, statisch typisierte, lazy Programmiersprache.  

Viele Buzzwords, was bedeuten sie:

Rein funktional: Funktionen hängen nur von ihren Inputs ab, sie haben keine Seiteneffekte. Variablen können nicht neu zugewiesen/mutiert werden.

Statisch typisiert: Jeder Ausdruck hat einen Typ, und der Compiler überprüft ob die Typen zueinander passen.
Das heißt nicht dass jede Variable explizit annotiert werden muss, Haskell hat "type inference", also leitet Typen vom Kontext ab (wie Javas "var").

Lazy: Haskell wertet nur aus was tatsächlich verwendet wird. Das kann man sich so vorstellen wie wenn man in Java statt mit direkten Werten mit Lambdas ohne Inputs arbeitet (Supplier<T>).

## Was sind die Unterschiede zu OO oder imperativen Sprachen?

- In einer imperativen Sprache schreibt man eine Liste von Schritten die der Computer ausführen soll
- In einer funktionalen Sprache beschreibt man die Instruktionen auf Datenebene in einem eher deklarativen Stil

Beispiel:
Fakultät berechnen (wie man es üblicherweise machen würde) in Java:

public int factorial(int number) {
    int current = 1;
    for (int i = 1;i<=number;i++) {
        current = current * i;
    }
}

Fakultät berechnen in Haskell:

factorial :: Int -> Int
factorial n = product [1..n]

Der funktionale Stil tendiert dazu eher deklarativ zu sein - die Fakultät einer Zahl ist das Produkt aller Zahlen darunter, während der imperative Stil eher eine Liste an Schritten darstellt - was muss ich tun um die Fakultät einer Zahl auszurechnen.

## Ziele des Workshops

Für mich:
- Vorurteile gegen über FP und speziell Haskell abbauen:
  - Nicht esoterisch, sondern gut geeignet für viele Domains, z.B. web server
  - Man braucht keinen Mathedoktor um darin programmieren zu können
  - Relevante OOP Konzepte wie Polymorphism und Abstraction lassen sich genauso auch in Haskell finden

- Meine Faszination und mein Wissen an euch weitergeben

Für euch:
- ?
