(* Mathematica Package *)

(*****************************************************************************)
(*markdown!

*)
(*****************************************************************************)
BeginPackage["MathCraftAddOn`HTML`"]
Needs["MathCraftAddOn`System`"]

mcListToHTMLList::usage = "mcListToHTMLList[list] converts a List
expression to an HTML ul element."

Begin["`Private`"]


(*****************************************************************************)
(* ::Section:: *)
(* Constants *)



(*****************************************************************************)
(* ::Section:: *)
(* mcListToHTMLList *)

Options[mcListToHTMLList] = {
    "ToStringFunction" -> Automatic
}

mcListToHTMLList::notstr = "List element `1` is not stringified.";

mcListToHTMLList[li_List, OptionsPattern[]] := Composition[
    # <> "\n" &,
    "<html>\n" <> # <> "\n</html>" &,
    "<ul>\n" <> # <> "\n</ul>" &,
    StringJoin,
    Riffle[#, "\n"]&,
    ("    <li>" <> ToString[#] <> "</li>") & /@ # &,
    If[
        !MatchQ[#, {___String}],
        Message[mcListToHTMLList::notstr, Select[#, !StringQ[#]&, 1]];
        Return[$Failed],
        #
    ]&,
    If[
        OptionValue["ToStringFunction"] === Automatic,
        If[StringQ[#], #, ToString[#]]& /@ #,
        OptionValue["ToStringFunction"] /@ #
    ]&
][li]





End[]

EndPackage[]
