
module Example1 where

data Decl = Decl Label TypeInCtx [DnTerm Label]
data Clause var = Clause [Pat var] [Morphism var] [Pat var]
data Morphism var = [Pat var] Label [Pat var]
data Pat var = Var var | Pred Label [Pat var]

-- Nat >: even :> Bool
-- { Zero >- even -> True 
--   Succ(n) >- even -> r :=
--		n >- odd -> r
-- }
even = Decl $
	"even" $
	([], Pi (Pred "Nat" []) (Pred "Bool" [])) $
	[
		Clause 	[Pred "Zero" []] [] [Pred "True" []]
	,	Clause  [Pred "Succ" [Var "n"]]
			[Morphism [Var "n"] "odd" [Var "r"]]
				[Var "r"]
	]

odd = Decl $
	"odd" $
	([], Pi (Pred "Nat" []) (Pred "Bool" [])) $
	[
		Clause 	[Pred "Zero" []] [] [Pred "False" []]
	,	Clause  [Pred "Succ" [Var "n"]]
			[Morphism [Var "n"] "even" [Var "r"]]
				[Var "r"]
	]

-- a : Type :- a >: unit :> List[a]
-- { x >- unit -> Cons(x,Nil) }
unit = Decl $
	"unit" $
	([UpType], Pi (Var 0) (Pred "List" [Var 0])) $
	[
		Clause [Var "x"] [] [Pred "Cons" [Var "x", Pred "Nil" []]]
	]

--
bool = Decl $
	"Bool" $
	([], UpType) $
	[]

true = Decl $
	"True" $
	([], UpPred "Bool" []) $
	[	Clause [] [] [DnPred "True" []]
	]
