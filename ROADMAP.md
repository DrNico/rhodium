## ROADMAP

### Phase 0: µRhodium
- [x] `(micro)`   interpreter for simply-typed arrow calculus

### Phase 1: the Strap
- [ ] `(strap)`   improved parser with: location, layout rules, error reports
- [ ] `(strap)`   type system: dependant types, with skeleton for Homotopy Types in the future
- [ ] `(kernel)`  develop a crude memory model (unique pointers, all copying is deep)
- [ ] `(strap)`   generate LLVM code

### Phase 2: the Boot
- [ ] `(core)`    develop a core library: basic data-types, parsing, etc.
- [ ] `(boot)`    foreign-function interface
- [ ] `(boot)`    re-write the strap in Rhodium, have it compiled by boot

### Phase 3: the Bootstrap
- [ ] `(rhc-bs)`  executable produced by compiling the Rhodium compiler source using the Boot

### Phase 4: the Rhodium compiler
- [ ] `(rhc)`     memory management, garbage collection

### Parser suggestion:
The new parser should parse new-style definitions such as

	Bool >: even :> Bool {
		Zero >- even -> True .
		Succ(x) >- even -> r :=
			x >- odd -> r .
	}

#### Some definitions
  * **strap** The initial tool transforming Rhodium code into LLVM code for compilation. The strap is written in Haskell. It may use some source code from the kernel as a library.
  * **boot** The minimum Rhodium code required to parse and compile other Rhodium code into LLVM. The boot does essentially the same thing as strap, but is written in Rhodium source language.
  * **kernel** The library, used by both strap and boot tools, written in Rhodium, providing the basic functionality for compiling and interpreting Rhodium code. The strap is the first instance transforming the source code of the kernel into an actual linkable library. The boot links against that library.
  * **make** Building sequences are triggered by the invocation of the make tool.
  * **core** The Rhodium standard library.

#### Type System
Rhodium has a Dependant Type System, where types can depend on both other types and values of other types. In the kernel, the type system is implemented on top of Contextual Categories, where objects are contexts and morphisms are typed terms. On top of that lies a Category of Judgements. The system is a faithful implementation of the concepts laid out in [arxiv:1211.2851v2](http://arxiv.org/abs/1211.2851v2).

#### TODO
- [x] objects, morphisms, composition in Contextual Categories
- [x] projections, pullbacks
- [x] dependant function
- [ ] dependant pair, one, zero, sum types
- [ ] judgemental equality
- [ ] a `Check` monad, validating all objects at injection
- [ ] type-check from source language
