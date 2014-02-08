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
    mcMatrixDiagonals[{}]
    ,
    TestID->"TestList-20130806-D1S1V5-mcMatrixDiagonals"
]

Test[
    mcMatrixDiagonals[{1,2}]
    ,
    mcMatrixDiagonals[{1,2}]
    ,
    TestID->"TestExample-20130805-H8R1D9-mcMatrixDiagonals"
]
