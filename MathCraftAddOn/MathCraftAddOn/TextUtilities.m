(* ::Package:: *)

(* Mathematica Package *)
(*
 by jiecaoc@wolfram.com
 07/26/13
*)

BeginPackage["MathCraftAddOn`TextUtilities`"]

mcTextDiff::usage = "mcTextDiff[oldText, newText] return a gird of formatted diff result of two texts"
mcReplaceUnicode::usage = "mcReplaceUnicode[text] store the encode for unicode which was broken by Import[..]"

Begin["`Private`"]

Clear[DiffList];
DiffList[list1_List, list2_List] :=
    Module[
	{tmp},
	Flatten[#, 1]&@
	(
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
Clear[AddStyle];
Options[AddStyle] = 
    {
	"Style" -> {
                     LightGreen,
                     LightRed,
	                {LightPurple, LightBlue}
                   }
    };

AddStyle[list_List, OptionsPattern[]]:=
    Module[
	{
	    keptColor = First@OptionValue["Style"],
	    removedColor = (OptionValue["Style"])[[2]],
	    changedColors = Last@OptionValue["Style"],
		removedStyle = {Bold, FontVariations -> {"StrikeThrough" -> True}}
	},
	Switch[
	    list,
	    {a_,a_}, list,
	    {"",_}, {Framed[#, Background -> removedColor]& @ Style[Last[list], Sequence@@removedStyle], 
		     Framed[#, Background -> keptColor]& @ Style[Last[list], Bold]},
	    {_,""}, {Framed[#, Background -> keptColor]& @ Style[First[list], Bold],
		     Framed[#, Background -> removedColor]& @ Style[First[list], Sequence@@removedStyle]},
	    {_,_}, {Framed[#, Background -> First@changedColors]& @ Style[First[list], Bold],
		   Framed[#, Background -> Last@changedColors]& @ Style[Last[list], Bold]}
	]
   ]




Clear[mcTextDiff];
Options[mcTextDiff] = {"RestoreUnicode" -> True}
mcTextDiff[oldText_String,newText_String, OptionsPattern[]]:=
    Module[
	{tmp,cmpStrings},
	tmp = DiffList[StringSplit[oldText,"\n"], StringSplit[newText,"\n"]];
	cmpStrings[s1_,s2_] := Replace[SequenceAlignment[s1,s2],a_String:>{a,a},1];
	tmp = Composition[
	         (Row /@ Transpose[#])& /@ #&,
	         (AddStyle /@ #&) /@ #&,
	         (cmpStrings[First[#], Last[#]]& /@ #)&
	      ][tmp];
	Grid[tmp, ItemSize->Fit, Alignment->Left, Frame->All, FrameStyle->LightGray]
    ]

End[]


EndPackage[]
