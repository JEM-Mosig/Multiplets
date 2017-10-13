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


(* Tableau                                     *)
(* =========================================== *)

(* print visual representation in StandardForm if small enough spec *)
Format[t:Tableau[spec_, ___], StandardForm] :=
Interpretation[
  tableauChart[t],
  t
] /; Total[spec] <= $MaxTableauPrintPoints

(* always print visual representation in TraditionalForm *)
Format[t:Tableau[spec_, ___], TraditionalForm] :=
Interpretation[
  tableauChart[t],
  t
]


(* tableauChart (PRIVATE)                      *)
(* =========================================== *)

SyntaxInformation[tableauChart] = {
  "ArgumentsPattern" -> {_}
};

tableauChart[Tableau[spec_, filling_:{}]] := If[spec == {},
(* an empty Tableau is displayed as "1" *)
  Style[1, Large, Bold],
(* a non-empty Tableau is displayed as a diagram *)
  Grid[
    Module[{stack},
      If[ListQ[filling],
        stack = Prepend[filling /. (Indeterminate|None|Null) -> Invisible[1], 0],
        stack = Prepend[filling[Tableaux[spec]] /. (Indeterminate|None|Null) -> Invisible[1], 0]
      ];
      Table[
        If[j <= spec[[i]] && Length[stack] > 1, First[stack = Rest[stack]], Invisible[1]],
        {i, Length[spec]},{j, Max[spec]}
      ]
    ],
    Frame -> {
      None, None,
      Flatten[
        Table[
          {i,j} -> ((j <= spec[[i]])/.{
            True -> Directive[Black, Thick],
            False -> {}
          }),
          {i, Length[spec]}, {j, Max[spec]}
        ]
      ]
    },
    FrameStyle -> Directive[LightGray, Thin],
    ItemSize -> All
  ]
]


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