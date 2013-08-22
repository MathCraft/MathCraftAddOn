(* Mathematica Package *)

BeginPackage["MathCraftAddOn`System`"]

mcFalseQ::usage = "mengFalseQ[expr] yields True if expr is False, and yields False otherwise.  mcFalseQ[args] is NOT equivalent to Not[TrueQ[args]] for all args."

Begin["`Private`"]

Clear[mcFalseQ]

mcFalseQ[False] = True;

mcFalseQ[True] = False;

mcFalseQ[_] = False;

End[]
EndPackage[]
