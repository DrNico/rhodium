# Rhodium
Rhodium is a programming language, currently in development, designed with the following goals in mind:

  * powerful type inference
  * complex tasks expressed in concise code
  * fast execution

that is, the goals of pretty much every language out there.

### Why?
So *why* yet another language? Here is the Creator's personnal opinion, built up over ten years of toying 
around with many languages in an attempt to use them for serious mathematics and engineering.

  * Because Mathematica, supposed to be a platform for Mathematics, is suprisingly unsound and kludgy.
    The non-commutative multiplication operator is simply defective. As a programming language it is
    not interesting at all.
  * Because Python, although a great scripting language, is out of its depth in computing-intensive and
    rigorous mathematical tasks. The immense Sage package for mathematics is a great demonstration of a giant kludge.
    Python's "duck-typing" system is fun to use and versatile, but not rigorous enough for serious math.
  * Because C++ suffers from its respectable age of a half-century, is painfully verbose,
    and generally is becoming more complex as it is being "improved".
  * Because Swift, although a recent and amazing object-oriented imperative language (the best, in my opinion), is still
    a child of the old imperative programming tradition, and it is just time to move on.
  * Because Haskell, the flagship functional programming language, has been great in establishing the power of this
    style of programming, but is also showing its age. It is also becoming very complex as it is "improved", basically
    always requiring several GHC extensions to use with the latest programming techniques.
  * Because Coq and other proof-assistants, although impressive in their reasoning power, have been designed
    by mathematicians, for mathematicians, and are not appealing as programming languages.

Based on this historical account, the Creator thinks it is time to start from scratch with 21st century ideas: **Rhodium**.

### What?
So what should a new language look like? It should have:

  * Type inference, relieving the programmer of writing a lot of type annotations
  * Modern and simple syntax, imperative style belongs to the last century
  * Compilation to machine code for wicked-fast execution
  * On-the-fly compilation, possibly into bit-code, for dynamic programming

### How?
In an attempt to achieve the design goals, Rhodium is based on three exciting recent developments in Computer Science:

  * Homotopy of Type Theory is an hot research topic in theoretical Computer Science, tying together
    type theory, homotopy (roughly geometry), and logic. A type system based on this theory can of course
    check if a program is type-correct, however it can also check, and eventually prove by itself, logical proofs.
    It makes heavy use of the correspondence: types are logical predicates, programs are proofs.
  * LLVM is a collection of modular and reusable compiler and toolchain technologies. It is a modern, fast,
    optimizing back-end for compiling to numerous targets. It is a very active project, winner of the
    2012 Software System ACM award, and is used by Apple as its compiler back-end for both OSX and iOS software.
  * Arrows inherited their name from Category Theory, a branch of mathematics especially well suited to
    describe programs and their effects. They suggest an interesting syntax, implemented as an extension in several Haskell
    compilers, for concise programming in the presence of side-effects, error recovery, state manipulation,
    stream manipulation and many other areas. Arrows take a central place in Rhodium, much as Monads take a central
    place in Haskell for imperative-like programming in a purely functional language.

The main challenge in developing Rhodium is to put together rigorous theoretical concepts and a modern high-performance
compilation system, to produce a language that is both simple to use and solidly based on sound, deep, mathematical
concepts. Rhodium is meant to be great for "programming in the large", while having at its core a powerful
modern type system.

### When?
A working interpreter is expected by mid-2015, and a compiler by the end of 2015. The first stable release of a decent
compiler with a decent memory allocator will probably have to wait until 2016. Until that time, all the details of the
language, its syntax, execution model, memory model, are in flux.
