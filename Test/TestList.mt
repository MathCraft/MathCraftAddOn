(* Mathematica Test File *)

Test[
	mcMatrixDiagonals[{{1, I}, {-I, 1}}]
	,
	{{I}, {1, 1}, {-I}}
	,
	TestID->"TestExample-20130805-H8R1D9-mcMatrixDiagonals"
]


Test[
	mcMatrixDiagonals[{}]
	,
	{}
	,
	TestID->"TestExample-20130805-H8R1D9-mcMatrixDiagonals"
]

(*Test[
	mcMatrixDiagonals[{1,2}]
	,
	$Failed
	,
	TestID->"TestExample-20130805-H8R1D9-mcMatrixDiagonals"
]*)