(* Mathematica Package *)

BeginPackage["MathCraftAddOn`System`"]

mcFalseQ::usage = "mcFalseQ[args] is a shorthand of Not[TrueQ[args]]"

Begin["`Private`"]

mcFalseQ[args___] := !TrueQ[args];


End[]
EndPackage[]
