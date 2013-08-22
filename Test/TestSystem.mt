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
