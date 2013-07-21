(* Mathematica Package *)

(*****************************************************************************)
(*Markdown!

*)
(*****************************************************************************)
BeginPackage["MathCraftAddOn`Timing`"]

\[ScriptM]Timing::usage = "\[ScriptM]Times[program, timingFunc, timingLimit, iterationLimit] "

Begin["`Private`"]


(*****************************************************************************)
(* ::Section:: *)
(* \[ScriptM]Timing *)
(*

todo:
* don't waste evaluations
*)

ClearAll[\[ScriptM]Timing]

Attributes[\[ScriptM]Timing] = {HoldAll};

Options[\[ScriptM]Timing] = {
	"ClearSystemCacheQ" -> True

};

\[ScriptM]Timing[program_, timingFunc_: Timing, timingLimit_: 60, iterationLimit_: 10^5, OptionsPattern[]] := Module[
    {
        totalTime = 0,
        totalIterationLog = 0,
        (*totalTimeRes, totalIterationLogRes,*)
        normalizedTimeLimit = Power[10, Floor[Log[10, timingLimit]]],
        res,
        totalIterations = Power[10, #] & /@ Range[0, Floor[Log[10, iterationLimit]]],
        nestDepth = If[$VersionNumber < 6, 2, 1] (*< in v5 and earlier, First[Timing[expr;]] /. Second->1 yields just the number of seconds required for the evaluation of expr.  *)
    },
    Catch[
        If[
            Or[
            	(
	            	totalTime = Nest[
	                    First,
	                    totalIterationLog ++;
	                    If[OptionValue["ClearSystemCacheQ"], ClearSystemCache[]];
	                    timingFunc @ (Do[res = program, {#}];),
	                    nestDepth
	                ]
	            ) >= normalizedTimeLimit,
	            # === totalIterations[[-1]]
            ],
            Throw[{totalTime, totalIterationLog}],
            Print[StringForm["ran `1` iterations for a total time of `2` seconds", totalIterations[[totalIterationLog]], ToString[SetPrecision[totalTime, 3]]]]
        ] & /@ totalIterations
    ] /. {totalTimeRes_, totalIterationLogRes_} :> (
    	Print[StringForm["estimating based on `1` iterations run for a total time of `2` seconds", totalIterations[[totalIterationLogRes]], ToString[SetPrecision[totalTimeRes, 3]]]];
    	{totalTimeRes / totalIterations[[totalIterationLogRes]], res}
    )
];



End[]

EndPackage[]
