BeginPackage["MathCraftAddOn`Formatting`"]
Needs["MathCraftAddOn`System`"]




Begin["`Private`"]


ClearAll[mcExpressionToString];
(*!

Example:
    In[289]:= expression := {
        "e1" -> {"p1" -> 1, "p2" -> {1, "x1"}, {"Internal", "p"} -> {1, "x1"}, "FooBar" -> "XXX"},
        "e2" -> {"p1" -> 2, "p2" -> {2, "x2"}},
        "e3" -> {"p1" -> 3},
        "e4" -> {"p3" :> If[EvenQ[n],"even","odd"]},
        "e5" -> {},
        "e6",
        If[EvenQ[n],"even","odd"]
    };

    mcExpressionToString[expression]
    Out[290]=
{
    "e1" -> {
        "p1" -> 1,
        "p2" -> {
            1,
            "x1"
        },
        {
            "Internal",
            "p"
        } -> {
            1,
            "x1"
        },
        "FooBar" -> "XXX"
    },
    "e2" -> {
        "p1" -> 2,
        "p2" -> {
            2,
            "x2"
        }
    },
    "e3" -> {
        "p1" -> 3
    },
    "e4" -> {
        "p3" :> If[EvenQ[n], "even", "odd"]
    },
    "e5" -> {

    },
    "e6",
    "odd"
}


In[312]:= mcExpressionToString[{1, {b, c}}]

Out[312]= "{
    1,
{
        b,
        c
    }
}"

*)
$mcTabCharacter = "    ";

ClearAll[mcExpressionToString];
With[
    {$tab=$mcTabCharacter},
    mcExpressionToString[atom: Except[_Rule | _RuleDelayed | _List], indentLevel_:0] := StringJoin[
        StringJoin[Table[$tab, {indentLevel}]],
        StringReplace[ToString[Hold[atom], InputForm, CharacterEncoding -> "ASCII"], RegularExpression["^Hold\\[(.*)\\]$"] -> "$1"]
    ];
    mcExpressionToString[rule: _Rule | _RuleDelayed, indentLevel_:0] := Module[
        {lhs, rhs, head, padding = StringJoin[Table[$tab, {indentLevel}]]},
        {lhs, rhs, head} = Extract[rule, {{1}, {2}, {0}}, Hold];
        head = If[head === Hold[RuleDelayed], " :> ", " -> "];
        {lhs, rhs} = If[
            MatchQ[#, Verbatim[Hold][List[___]]],
            With[{list = Extract[#, {1}]}, mcExpressionToString[list, indentLevel]],
            StringReplace[ToString[#, InputForm, CharacterEncoding -> "ASCII"], RegularExpression["^Hold\\[(.*)\\]$"] -> "$1"]
        ]& /@ {lhs, rhs};
        StringJoin[{padding, lhs, head, rhs}]
    ];
    mcExpressionToString[list_List, indentLevel_:0] := Module[
        {padding = StringJoin[Table[$tab, {indentLevel}]]},
        StringJoin[
            "{\n",
            StringJoin@Riffle[mcExpressionToString[#, indentLevel+1]& /@ list, ",\n"],
            "\n",
            padding,
            "}"
        ]
    ];
]





End[]

EndPackage[]