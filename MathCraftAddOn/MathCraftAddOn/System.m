(* Mathematica Package *)

BeginPackage["MathCraftAddOn`System`"]

mcFalseQ::usage = "mengFalseQ[expr] yields True if expr is False, and yields False otherwise.  mcFalseQ[args] is NOT equivalent to Not[TrueQ[args]] for all args."
mcNestedWith::usage = "mcNestedWith[{x=x0, y=y0, ...}, expr] is like mcNestedWith[{x=x0, y=y0, ...}, expr] but allows y0 to be a function of x0"
Begin["`Private`"]

(*markdown!

  mcFlaseQ

*)
Clear[mcFalseQ]

mcFalseQ[False] = True;

mcFalseQ[True] = False;

mcFalseQ[_] = False;


(*markdown!


## References

* http://www.wolfram.com/mathematica/new-in-10/inactive-objects/transform-code.html
* http://mathematica.stackexchange.com/questions/10432/how-to-avoid-nested-with
* http://www.wolfram.com/mathematica/new-in-10/inactive-objects/study-maxwells-equations.html

*)

SetAttributes[mcNestedWith, HoldAll]

mcNestedWith[expr_] := Activate[
    ReplaceRepeated[
        Inactivate[expr],
        Inactive[With][{first_, rest__}, body_] :> Inactive[With][{first}, Inactive[With][{rest}, body]]
    ]
]

End[]
EndPackage[]
