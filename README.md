# EPS
Evolutionary Program Sketching

Evolutionary Program Sketching (EPS) is a program synthesis method combining Genetic Programming (GP) and formal synthesis based on checking satisfiability of an SMT formula encoding the program. 
SMT stands for Solvability Modulo Theories, and is an extension of the first-order logic in which formulas may use expressions from certain theories.
For example, the theory of integer arithmetic allows to construct formulas such as ` (and (> x 0) (y > 0))`.
The task of the solver is to find such a valuation of all variables (the *model*), which satisfies all constraints.

EPS works by evolving program trees containing holes.
Holes are categorized with regard to the content that may fill them.
Currently there are two types of holes available: constant holes (may be filled with constant of the certain type) and variable holes (may be filled with any input variable of the synthesized function).
For more details you can read the paper specified in the *How to cite* section.


## How to build
Scala must be installed on the system. Two methods for building are currently supported:
* **Eclipse/ScalaIDE project**. Just import the project in the IDE.
* **SBT**. *build.sbt* is configured so that fuel and swim folders are assumed be placed at the same directory level as the directory containing EPS folder downloaded from repository. You can just type in the command line: `sbt package` to produce jar for EPS in the *target/scalaX.Y* directory.

## How to run
Scripts for running main configurations of EPS are stored in the *scripts* folder.
Jars of fuel, swim and EPS must be on the classpath (example in the scripts).
pysv directory is specified with the `--eps.pathToPySV` option.


## Dependencies
* [FUEL](https://github.com/kkrawiec/fuel) - main evolution engine.
* [SWIM](https://github.com/kkrawiec/swim) - GP utilities for FUEL.
* [pysv](https://github.com/iwob/pysv) - construction of queries to the SMT solver in the SMT-LIB language.


## How to cite

```.bib
@Inbook{Błądek2017,
  author="B{\l}{\k{a}}dek, Iwo
  and Krawiec, Krzysztof",
  editor="McDermott, James
  and Castelli, Mauro
  and Sekanina, Lukas
  and Haasdijk, Evert
  and Garc{\'i}a-S{\'a}nchez, Pablo",
  title="Evolutionary Program Sketching",
  bookTitle="Genetic Programming: 20th European Conference, EuroGP 2017, Amsterdam, The Netherlands, April 19-21, 2017, Proceedings",
  year="2017",
  publisher="Springer International Publishing",
  address="Cham",
  pages="3--18",
  isbn="978-3-319-55696-3",
  doi="10.1007/978-3-319-55696-3_1",
  url="http://dx.doi.org/10.1007/978-3-319-55696-3_1"
}
```
