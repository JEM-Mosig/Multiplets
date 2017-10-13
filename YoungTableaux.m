(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: YoungTableaux *)
(* :Context: YoungTableaux` *)
(* :Author: jem-mosig@protonmail.com *)
(* :Date: 2017-10-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.1 *)
(* :Copyright: (c) 2017 jem-mosig@protonmail.com *)
(* :Keywords: Young-tableaux, group theory, quantum physics *)
(* :Discussion: *)

BeginPackage["YoungTableaux`"]

If[!ValueQ[Tableau::usage],
  Tableau::usage = "Tableau[spec, fil] is a wrapper for a general Young Tableau. spec is a list of row-lengths and fil specifies what is written within the boxes. Alternatively fil can be a filling function or nothing.";
];

If[!ValueQ[TableauQ::usage],
  TableauQ::usage = "TableauQ[expr] checks, whether expr is a well defined Young Tableau.";
];

If[!ValueQ[TableauDimension::usage],
  TableauDimension::usage = "TableauDimension[tab, n] gives the dimension of the Young Tableau tab for the SU(n) case.";
];

If[!ValueQ[TableauLetters::usage],
  TableauLetters::usage = "TableauLetters[tab, n] fills a given Tableau tab with one letter in each row, starting from the nth letter in the latin alphabet.";
];

If[!ValueQ[TableauDistances::usage],
    TableauDistances::usage = "TableauDistances[tab] fills a given Tableau tab with (possibly negative) integer 'distances'.";
];

If[!ValueQ[TableauHooks::usage],
    TableauHooks::usage = "TableauHooks[tab] fills a given Tableau tab with hook distances.";
];

If[!ValueQ[$MaxTableauPrintPoints::usage],
    $MaxTableauPrintPoints::usage = "$MaxTableauPrintPoints is an integer which controls up to what number of points a Tableau is presented graphically in StandardForm.";
];

Begin["`Private`"]


(* $MaxTableauPrintPoints                      *)
(* =========================================== *)

(* set the default value *)
$MaxTableauPrintPoints = 300;

(* prevent the user from setting invalid values *)
$MaxTableauPrintPoints /: HoldPattern[(Set | SetDelayed)[$MaxTableauPrintPoints, x_]] := (
  Message[$MaxTableauPrintPoints::intOnly];
  $MaxTableauPrintPoints
) /; (!MemberQ[{Integer, DirectedInfinity}, Head[x]] || Negative[x])

$MaxTableauPrintPoints::intOnly = "$MaxTableauPrintPoints must be a non-negative integer or Infinity.";


(* TableauQ                                    *)
(* =========================================== *)

SetAttributes[TableauQ, {}];

SyntaxInformation[TableauQ] = {
  "ArgumentsPattern" -> {_}
};

TableauQ[expr_] := And[
  Head[expr] === Tableau,
  ListQ[expr[[1]]],
  OrderedQ[Reverse[expr[[1]]]],
  Or[
    Length[expr] == 1,
    Length[expr] == 2 && Length[expr[[2]]] <= Total[expr[[1]]] && ListQ[expr[[2]]]
  ]
];

End[] (* `Private` *)

EndPackage[]