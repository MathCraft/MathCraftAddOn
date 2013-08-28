(*! markdown
This package will be designed as an OOP framework for Mathematica
*)

(*! markdown
 example:

 ClassDeClare[
    Person,
    Age,
    Name,
    SayHi[s_String]
 ]

 ClassDefine[
    Person,
    Age = 10
 ]

 ClassDefine[
    Person,
    Name = "Jack"
 ]

 ClassDefine[
    Person,
    SayHi[s_String] := "Hello " <> s
 ]

 person1 = ClassNew[Person];
 person1.Name
 person1.SayHi["Dan"]
*)


(* Mathematica Package *)

BeginPackage["MathCraftAddOn`OOPFramework`", { "Notation`"}]
(* Exported symbols added here with SymbolName::usage *)
ClassDefine::usage = ""
ClassDeclare::usage = ""
ClassNew::usage = ""
Begin["`Private`"] (* Begin Private Context *)

(*
    set notation for member function call
*)
(*Notation[object_ . member_ \[DoubleLongRightArrow] object_[member_]]*)
Notation[ParsedBoxWrapper[
RowBox[{"object_", " ", ".", " ",
     "member_"}]] \[DoubleLongRightArrow] ParsedBoxWrapper[
RowBox[{"object_", "[", "member_", "]"}]]]


ClearAll[ClassDefine];
Attributes[ClassDefine] = {HoldAllComplete};
ClassDefine[className_,definition_] :=
    Module[
        {
            def = Hold[definition],
            lhs,
            rhs
        },
        (*seperate lhs and rhs*)
        Which[
            MatchQ[def,Verbatim[Hold][Verbatim[SetDelayed][l_,r_]]],
                (lhs = Replace[def,Verbatim[Hold][Verbatim[SetDelayed][l_,r_]]:>Hold[l]];
                rhs = Replace[def,Verbatim[Hold][Verbatim[SetDelayed][l_,r_]]:>Hold[r]]),
            True,
                (lhs = Replace[def,Verbatim[Hold][Verbatim[Set][l_,r_]]:>Hold[l]];
                rhs = Replace[def,Verbatim[Hold][Verbatim[Set][l_,r_]]:>Hold[r]])
        ];
        className[First@lhs]=.;
        {Hold[className[First@lhs]],rhs}/.{Verbatim[Hold][a_],Verbatim[Hold][b_]}:>Hold[a :=
                                                                                            b]/.a_:>First[a];
    ];

ClearAll[ClassDeclare];
Attributes[ClassDeclare] = {HoldAll};
ClassDeclare[className_,fields___] :=
    Module[ {},
        ClearAll[className];
        (className[#] :=
             Null)&/@(List@fields);
    ];

ClearAll[ClassNew];
ClassNew[className_]:=
    Module[
        {obj},
        DownValues[obj] = DownValues[className] /. Verbatim[HoldPattern][className[a___]] :> HoldPattern[obj[a]];
        Return[obj]
    ]

End[] (* End Private Context *)

EndPackage[]

