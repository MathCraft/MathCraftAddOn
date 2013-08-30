(*! markdown
    This package will be designed as an OOP framework for Mathematica
    by Jack mathcraft.jack@gmail.com
*)


(*! markdown
    TODO:
        + add self point
        + support point?
        + if possible, add security check
        + conponent
        + tighter type check
        + keep the assignment same as type-in   DONE

    ISSUE:
        + How to make symbol intrduce in ClassDeclare[..] and ClassDefine[..] local?
        + How to change the precedence of the operator Dot?
*)

(*! markdown
## Example:
ClassDeclare[
    A,
    size=10,
    area[]:= size^2,
    setSize[s_]
]
ClassDefine[
    A,
    setSize[s_]:=(size=s)
]

ClassDeclare[
    B<-A,
    perimeter[]
]

ClassDefine[
    B,
    perimeter[]:=size*4
]

ClassDeclare[
    class,
    height=10,
    obj = ClassNew[B],
    volume[]
]

ClassDefine[
    class,
    volume[]:= (obj.area[]) * height
]

Test[
    class.volume[]
    ,
    1000
    ,
    TestID->"Test-20130828-B9M7N4"
]

Test[
    newobj = ClassNew[class];
    newobj.volume[]
    ,
    1000
    ,
    TestID->"Test-20130829-J0R4X3"
]
*)


(* Mathematica Package *)

BeginPackage["MathCraftAddOn`OOPFramework`"(*, { "Notation`"}*)]
(* Exported symbols added here with SymbolName::usage *)
ClassDefine::usage = ""
ClassDeclare::usage = ""
ClassNew::usage = ""
ClassQ::usage = ""
seperateLhsRhs::usage = ""
getInternalName::usage = ""
callMember::usage = ""
(*define the operator . in the kernel level*)
Unprotect[Dot];
Dot[a_?((ObjectQ[#] || ClassQ[#])&), b_] := a[b];
Dot[a_?((ObjectQ[#] || ClassQ[#])&), b_, c__] := Dot[a, b][c];
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
$Obj
$Tag
(* ClassQ will return Ture for any class created by ClassDeclare
    or ClassNew
*)
ClassQ[a_] := a[$Tag] === $Class;
ObjectQ[a_] := a[$Tag] === $Obj;

ClearAll[ClassDefine];
Attributes[ClassDefine] = {HoldAllComplete};
ClassDefine[className_,definition_] :=
    Module[
        {
            lhs,
            op,
            rhs
        },
        (*seperate lhs and rhs*)
        {lhs, op, rhs} = seperateLhsRhs[Hold[definition],"HoldQ"->True];
        (* should replace member data as a downvalue of the class*)
        rhs = ReplaceAll[rhs, className[DataSet]];
        rhs = ReplaceAll[rhs, Verbatim[Hold][className[a__]] :> className[a]];
        className[First@lhs]=.;
        {Hold[className[First@lhs]], First@op, rhs}/.
            {Verbatim[Hold][a_], operator_, Verbatim[Hold][b_]}:>Hold[operator[a, b]]
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
Options[seperateLhsRhs] = {"HoldQ" -> False};
Attributes[seperateLhsRhs] = {HoldAllComplete};
seperateLhsRhs[exp_, OptionsPattern[]] :=
    Module[
        {tmp = Hold[exp], result},
        tmp = tmp /.Verbatim[Hold][a_Hold] :> a;
        result =
            Which[
                !MatchQ[tmp, Verbatim[Hold][_Set | _SetDelayed]],
                    {tmp, Hold[Null], Hold[Null]},
                MatchQ[tmp, Verbatim[Hold][_Set]],
                    Replace[tmp, Verbatim[Hold][Verbatim[Set][a_, b_]] :> {Hold[a], Hold[Set], Hold[b]}],
                True,
                    Replace[tmp, Verbatim[Hold][Verbatim[SetDelayed][a_, b_]] :> {Hold[a], Hold[SetDelayed], Hold[b]}]
            ];
        If[
            OptionValue["HoldQ"],
            result,
            First /@ result
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
        (*Attributes[className] = {HoldAll};*)
        ClassInheritFrom[className, baseClass];
        tmp2 = (First/@(seperateLhsRhs /@ tmp));
        dataSet = (# -> Hold[className[#]])& /@ (*tmp2*)Select[tmp2,(Head[#] === Symbol) &];
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
        obj[$Tag] := $Obj;
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





(* =====================================================
                    Internal functions
   ===================================================== *)
Clear[getInternalName];
Attributes[getInternalName] = {HoldAllComplete};
getInternalName[s_Symbol] := Symbol["OOP$" <> StringReplace[ToString[Hold[s]], "Hold["~~x__~~"]" :> x]];

 Clear[callMember];
 Attributes[callMember] = {HoldAll};
 callMember[objName_Symbol, member_] :=
    Module[
        {},
        objName[getInternalName[member]]
    ]






End[] (* End Private Context *)

EndPackage[]
