(* Mathematica Test File *)

Test[
    mcFalseQ[True] === False
    ,
    True
    ,
    TestID->"TestSystem-20130822-X1V5Z6"
]

Test[
    mcFalseQ[False] === True
    ,
    True
    ,
    TestID->"TestSystem-20130822-R0M0U4"
]

Test[
    mcFalseQ["foobar"] === False
    ,
    True
    ,
    TestID->"TestSystem-20130822-K8R8H4"
]

Test[
    mcNestedWith[{x=1, y=2x}, x+y]
    ,
    3
    ,
    TestID->"TestSystem-20140714-Y4V4P9"
]

Test[
    mcNestedWith[{x=Sin[2], y=-x}, x y]
    ,
    -Sin[2]^2
    ,
    TestID->"TestSystem-20140714-W4L2E4"
]
