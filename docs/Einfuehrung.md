# Einführung

## Was ist Haskell?

Haskell ist eine rein funktionale, statisch typisierte, lazy Programmiersprache.

Mittlerweile ist die Sprache über 30 Jahre alt, 1992 wurde die erste Version des Glasgow Haskell Compilers geschrieben
(der häufigst verwendete Haskell Compiler, der jetzt auch auf euren Rechnern ist).
Anfangs war die Programmiersprache rein akademisch, sie hat ihre Ursprünge in der Mathematik, aber wurde auch bald in kommerziellen Systemen verwendet.
Heutzutage wird die Weiterentwicklung von der Haskell Foundation geleitet, die von vielen Firmen gesponsert wird.

Die Sprache wird am häufigsten für Parser, Compiler, Webserver und Cryptocurrency verwendet, z.B. bei Meta/Facebook für Spam/Phishing Protection, GitHub für Source Code Parsing und Analyse, IOHK um die Cardano Blockchain Plattform und ihre Smart Contract Sprache zu implementieren.

Viele Buzzwords, was bedeuten sie:

Rein funktional: Funktionen hängen nur von ihren Inputs ab, sie haben keine Seiteneffekte. Variablen können nicht neu zugewiesen/mutiert werden.

Statisch typisiert: Jeder Ausdruck hat einen Typ, und der Compiler überprüft ob die Typen zueinander passen.
Das heißt nicht dass jede Variable explizit annotiert werden muss, Haskell hat "type inference", also leitet Typen vom Kontext ab (wie Javas "var").

Lazy: Haskell wertet nur aus was tatsächlich verwendet wird. Das kann man sich so vorstellen wie wenn man in Java statt mit direkten Werten mit Lambdas ohne Inputs arbeitet (Supplier<T>).

## Andere Eigenschaften der Sprache

### Native executables
GHC kompiliert Haskell Code zu nativen executables. In dieser Executable befindet sich immer eine multithreaded Haskell runtime. Es gibt andere Compile targets wie Web Assembly und JS, die sind aber deutlich weniger produktionsreif.

### Garbage collected
Wie in Java kümmert man sich nicht darum Memory zu allocaten.

### Language Extensions
GHC versteht per Default recht wenig Haskell Syntax. Viel Syntactic Sugar und Language Features werden durch `Language Extensions` gesteuert, die pro Projekt oder auch pro Datei an und aus getoggled werden können.
In diesem Projekt sind viele (die meisten von der Community akzeptierten) Features per Default angeschaltet.


## Wie ist dieses Repo aufgebaut

Dieses Repository wurde mit `stack new` aufgesetzt.
Dies generiert die nötigen Dateien für `stack`, ein Buildtool für Haskell (so wie Maven für Java):

- `package.yaml` deklariert libraries, executables, dependencies etc.
- The `src` folder usually contains your application code
- The `app` folder contains executables - files with `main` functions which may import your application code from `src`
- The `test` folder contains a test-suite. `Spec.hs` contains a preprocessor macro to discover all files in the `test` folder with a `Spec` postfix and run a function called `spec` defined inside of them (you can ignore that file, just know that you need to define a function called `spec` which type is `Spec` from `Test.Hspec` in each of your tests)
- `Setup.hs` is generated boilerplate needed by `stack` 


## Ziele des Workshops

Für mich:
- Vorurteile gegen über FP und speziell Haskell abbauen:
  - Nicht esoterisch, sondern gut geeignet für viele Domains, z.B. web applications
  - Man braucht keinen Mathedoktor um darin programmieren zu können
  - Relevante OOP Konzepte wie Polymorphism und Abstraction lassen sich genauso auch in Haskell finden

- Meine Faszination und mein Wissen an euch weitergeben

Für euch:
- ?
