(* Mathematica Package *)

(*****************************************************************************)
(*Markdown!

*)
(*****************************************************************************)
BeginPackage["MathCraftAddOn`List`"]
Needs["MathCraftAddOn`System`"]

mcLongestCommonContinuousSequence::usage = "mcLongestCommonContinuousSequence[s1, s2] find the longest common continuous sequence of two strings or two lists."

mcMatrixDiagonals::usage = "mcMatrixDiagonals[matrix, direction] gives the diagnols of matrix, with direction 1 meaning the NE-SW diagnols and -1 the NW-SE ones."




Begin["`Private`"]


(*****************************************************************************)
(* ::Section:: *)
(* Region Title *)

(*markdown!

## Examples

	m = Table[RandomInteger[], {5}, {5}];
	m // MatrixForm
	MatrixDiagonals[m]
*)
Clear[mcMatrixDiagonals];
mcMatrixDiagonals[matrix_, direction_:1]:= With[
	{m = Switch[direction, -1, Reverse/@ matrix, _, matrix]},
	Composition[
		Map[Part[m,##]& @@ # & @ # &, #, {2}]&,
		Join@@#&,
		{Most[Reverse[#]], Map[Reverse,#,{2}]}&,
		Table[
			DeleteCases[
				Transpose[{Range[#],Range[#]+i}],
				{___,x_/;x>#,___}
			],
			{i,Range[0,#-1]}
		]&,
		Replace[#,{{d_Integer}:> d,_-> Return[$Failed]}]&,
		Union,
		Dimensions
	][m]
]


(*****************************************************************************)
(* ::Section:: *)
(* LongestCommonSequence *)

mcLongestCommonContinuousSequence[s1_String, s2_String] := Last[SortBy[GatherBy[Cases[SequenceAlignment[s1, s2], _String], StringLength], StringLength[#[[1]]]&]];

mcLongestCommonContinuousSequence[l1_List, l2_List] := Last[SortBy[GatherBy[DeleteCases[SequenceAlignment[l1, l2], {_List, _List}], Length], Length[#[[1]]]&]];


End[]

EndPackage[]
