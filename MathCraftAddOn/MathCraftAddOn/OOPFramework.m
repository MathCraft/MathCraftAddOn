(*! markdown
    This package will be designed as an OOP framework for Mathematica
    by Jack mathcraft.jack@gmail.com
*)

(*! markdown
## Example:
    ClassDeclare[
        A,
        width,
        length
    ];

    ClassDefine[
        A,
        width = 10,
        length = 100
    ];

    // B inherit from A
    ClassDeclare[
        B <- A,
        area[]
    ];

    ClassDefine[
        B,
        area[] := width * length
    ]

    // now we have define A and B
    b = ClassNew[B];
    b.area[]
*)


(* Mathematica Package *)

BeginPackage["MathCraftAddOn`OOPFramework`"(*, { "Notation`"}*)]
(* Exported symbols added here with SymbolName::usage *)
ClassDefine::usage = ""
ClassDeclare::usage = ""
ClassNew::usage = ""
ClassQ::usage = ""
seperateLhsRhs::usage = ""
(*define the operator . in the kernel level*)
Unprotect[Dot];
Dot[a_?ClassQ, b_] := a[b];
Protect[Dot]


Class::SyntaxError = "Syntax Error in `1`"
Begin["`Private`"] (* Begin Private Context *)

(*
    set notation for member function call
    This is not a good idea, since it only works
    under front end
*)
(*Notation[object_ . member_ \[DoubleLongRightArrow] object_[member_]]*)
(*Notation[ParsedBoxWrapper[
RowBox[{"object_", " ", ".", " ",
     "member_"}]] \[DoubleLongRightArrow] ParsedBoxWrapper[
RowBox[{"object_", "[", "member_", "]"}]]]
*)
(*================================================================================
*)
$Class
$Tag
(* ClassQ will return Ture for any class/object created by ClassDeclare
    or ClassNew
*)
ClassQ[a_] := a[$Tag] === $Class;

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
        (* should replace member data as a downvalue of the class*)
        rhs = ReplaceAll[rhs, className[DataSet]];
        rhs = ReplaceAll[rhs, Verbatim[Hold][className[a__]] :> className[a]];
        className[First@lhs]=.;
        {Hold[className[First@lhs]],rhs}/.
            {Verbatim[Hold][a_],Verbatim[Hold][b_]}:>Hold[a := b]
            /.a_:>First[a];
    ];
(*support multi-definition in one ClassDefine[..]*)
ClassDefine[className_, definitions__] :=
    Module[
        {tmp},
        tmp = Hold[ClassDefine[className, #]]& /@ (List@@Hold/@ Hold[definitions]);
        Replace[tmp, Verbatim[Hold][a_[b_, Verbatim[Hold][c___]]] :> a[b,c],1];
    ]


(* ================================================================================
In ClassDeclare, a member without [..] should be considered as data rather than a function,
it is necessary to distinguaish data from functions. e.g.
ClassDeclare[
    Person,
    age,        ---> data
    SayHi[s_String]     ---> function
]
*)

(*$Object is an empty class*)
$Object = Null;
Clear[$Object];

ClearAll[seperateLhsRhs];
Attributes[seperateLhsRhs] = {HoldAllComplete};
seperateLhsRhs[exp_] :=
    Module[
        {tmp = Hold[exp]},
        tmp = tmp /.Verbatim[Hold][a_Hold] :> a;

        Which[
            !MatchQ[tmp, Verbatim[Hold][_Set | _SetDelayed]],
               {First[tmp], Null},
            MatchQ[tmp, Verbatim[Hold][_Set]],
                Replace[tmp, Verbatim[Hold][Verbatim[Set][a_, b_]] :> {a, b}],
            True,
                Replace[tmp, Verbatim[Hold][Verbatim[SetDelayed][a_, b_]] :> {a, b}]
        ]
    ]

ClearAll[ClassDeclare];
Attributes[ClassDeclare] = {HoldAll};
ClassDeclare[className_Symbol,fields___] := ClassDeclare[className <- $Object, fields];
ClassDeclare[className_Symbol <- baseClass_, fields___] :=
    Module[
        {dataSet, tmp = List@@(Hold /@ Hold[fields]), tmp2},
        If[
            !MatchQ[baseClass, _Symbol|_List],
            Message[Class::SyntaxError, "ClassDeclare"];
            Return[]
        ];
        ClearAll[className];
        ClassInheritFrom[className, baseClass];
        tmp2 = (First/@(seperateLhsRhs /@ tmp));
        dataSet = (# -> Hold[className[#]])& /@ Select[tmp2,(Head[#] === Symbol) &];
        (* repeated (compare with inherit members) will be overwrite*)
        (className[#] :=
             Null)&/@ tmp2;
        AppendTo[DownValues[className], HoldPattern[className[DataSet]] -> dataSet];
        (*set a tag to specify that className is a class*)
        className[$Tag] := $Class;
        (* now, let's support initialization *)
        tmp2 = Hold[ClassDefine[className, #]]& /@ Select[tmp, !FreeQ[#, _Set | _SetDelayed]&];
        tmp2 = Replace[tmp2,Verbatim[Hold][Verbatim[ClassDefine][a_,Verbatim[Hold][b_]]] :> ClassDefine[a,b],1];
    ];
(*============================================================================
*)
ClearAll[ClassNew];
ClassNew[className_?ClassQ]:=
    Module[
        {obj},
        DownValues[obj] = ReplaceAll[DownValues[className], className[a___] :> obj[a]];
        (*UpValues[obj] = ReplaceAll[UpValues[className], className[a___] :> obj[a]];*)
        Return[obj]
    ]


(* =========================================================
   for now, ClassInheritFrom will only be called inside ClassDeclare
*)
ClearAll[ClassInheritFrom]
ClassInheritFrom[className_, baseClass_] :=
    Module[
        {},
        AppendTo[
            DownValues[className],
            ReplaceAll[DownValues[baseClass], baseClass[a___] :> className[a]]
        ];
        DownValues[className] = DeleteDuplicates@DownValues[className];
    ]

(* support multi-inheritance*)
ClassInheritFrom[className_, baseClasses_List] := ClassInheritFrom[className, #]& /@ baseClasses;

End[] (* End Private Context *)

EndPackage[]
