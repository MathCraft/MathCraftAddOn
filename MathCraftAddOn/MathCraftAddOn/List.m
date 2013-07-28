(* Mathematica Package *)

(*****************************************************************************)
(*Markdown!

*)
(*****************************************************************************)
BeginPackage["MathCraftAddOn`List`"]
Needs["MathCraftAddOn`System`"]

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


End[]

EndPackage[]
