(* Mathematica Package *)

BeginPackage["MathCraftAddOn`System`"]

mcFalseQ::usage = "mengFalseQ[expr] yields True if expr is False, and yields False otherwise.  mcFalseQ[args] is NOT equivalent to Not[TrueQ[args]] for all args."

mcIteratedWith::usage = "mcIteratedWith[{x=x0, y=y0, ...}, expr] is like With[{x=x0, y=y0, ...}, expr] but allows y0 to be a function of x."

mcLet::usage = "mcLet[{x = x0, y = y0, ...}, expr] is similar as mcLet[{x = x0, y = y0, ...}, expr] but XXX."

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

SetAttributes[mcIteratedWith, HoldAll]

mcIteratedWith[args_, expr__] := Activate[
    ReplaceRepeated[
        Inactivate[With[args, expr]],
        Inactive[With][{first_, rest__}, body_] :> Inactive[With][
            {first},
            Inactive[With][{rest}, body]
        ]
    ]
]



(*markdown!


## References

* http://www.wolfram.com/mathematica/new-in-10/inactive-objects/transform-code.html
* http://mathematica.stackexchange.com/questions/10432/how-to-avoid-nested-with
* http://www.wolfram.com/mathematica/new-in-10/inactive-objects/study-maxwells-equations.html

*)

ClearAll[mcLet];

SetAttributes[mcLet, HoldAll];

SyntaxInformation[mcLet] = {
    "ArgumentsPattern" -> {_, _},
    "LocalVariables" -> {"Solve", {1, Infinity}}
};

mcLet /: Verbatim[SetDelayed][lhs_, rhs : HoldPattern[mcLet[{__}, _]]] :=
    Block[
        {With},
        Attributes[With] = {HoldAll};
        lhs := Evaluate[rhs]
    ];

mcLet[{}, expr_] := expr;

mcLet[{head_}, expr_] := With[{head}, expr];

mcLet[{head_, tail__}, expr_] := Block[
    {With},
    Attributes[With] = {HoldAll};
    With[{head}, Evaluate[mcLet[{tail}, expr]]]
];

End[]
EndPackage[]
