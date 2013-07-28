(* ::Package:: *)

(* Mathematica Package *)
(*
by Jack <mathcraft.jack@gmail.com>
2013-07-26
*)

BeginPackage["MathCraftAddOn`TextUtilities`"]

mcTextDiff::usage = "mcTextDiff[oldText, newText] return a grid of formatted diff result of two texts"
mcReplaceUnicode::usage = "mcReplaceUnicode[text] restore the encode for unicode which was not interpreted by Import[..]"

Begin["`Private`"]

Clear[DiffList];
DiffList[list1_List, list2_List] := Module[
	{tmp},
	Flatten[#, 1] & @ (
		Switch[
			#,
			{{}, _List}, {"", #}& /@ Last[#],
			{_List, {}}, {#, ""}& /@ First[#],
			{_List, _List}, tmp = Max[Length/@#]; Transpose@(PadRight[#,tmp,""]& /@ #),
			_List, {#, #}& /@ #
		]& /@ SequenceAlignment[list1,list2]
    )
]


Clear[replaceUnicode];
mcReplaceUnicode[s_String]:=StringReplace[s,RegularExpression["(?i)\\\\:([0-9a-f]{4})"]:>FromCharacterCode[FromDigits["$1",16]]];


(* Input should be a list which includes two strings, e.g: {"xxafd", "aferaw"} *)
(*
   If you want change the background of the diff result, simply configure the
   option of AddStyle. more complex formatting require modification of AddStyle
*)
Clear[AddStyle];
Options[AddStyle] = {
	"Style" -> {
        LightGreen,
        LightRed,
	    {LightPurple, LightBlue}
    }
};

AddStyle[list_List, OptionsPattern[]] := Module[
	{
	    addColor = First@OptionValue["Style"],
	    removedColor = OptionValue["Style"][[2]],
	    changedColors = Last@OptionValue["Style"],
		removedStyle = {Bold, FontVariations -> {"StrikeThrough" -> True}},
		diff
	},
	diff = Switch[
	    list,
	    {a_, a_}, list,
	    {"", _},
    	{
	    	Framed[#, Background -> removedColor]& @ Style[Last[list], Sequence@@removedStyle],
		    Framed[#, Background -> addColor]& @ Style[Last[list], Bold]
	    },
	    {_, ""},
    	{
    		Framed[#, Background -> addColor]& @ Style[First[list], Bold],
		    Framed[#, Background -> removedColor]& @ Style[First[list], Sequence@@removedStyle]
	    },
	    {_, _},
    	{
    		Framed[#, Background -> First@changedColors]& @ Style[First[list], Bold],
		    Framed[#, Background -> Last@changedColors]& @ Style[Last[list], Bold]
	    }
    ];
    diff
]



(*****************************************************************************)
(* ::Section:: *)

(*markdown!

*)
Clear[mcTextDiff];
Options[mcTextDiff] = {}
mcTextDiff[oldText_String, newText_String, OptionsPattern[]] := Module[
	{tmp, cmpStrings},
	tmp = DiffList[StringSplit[oldText,"\n"], StringSplit[newText,"\n"]];
	cmpStrings[s1_,s2_] := Replace[SequenceAlignment[s1,s2], a_String :> {a,a}, 1];
	tmp = Composition[
		With[{rowIndex = {Item[#, Alignment -> Center]} & /@ Range[Length[#]]}, Join[rowIndex, #, rowIndex, 2]]&,
		(Row /@ Transpose[#])& /@ #&,
        (AddStyle /@ #&) /@ #&,
        (cmpStrings[First[#], Last[#]]& /@ #)&
    ][tmp];
	Grid[tmp, ItemSize -> Fit, Alignment -> Left, Dividers -> All, FrameStyle -> LightGray]
]

End[]

EndPackage[]
