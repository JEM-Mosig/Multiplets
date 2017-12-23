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

ClearAll["YoungTableaux`*"]

If[!ValueQ[Tableau::usage],
  Tableau::usage = "Tableau[spec, fil] is a wrapper for a general Young Tableau. spec is a list of row-lengths and fil specifies what is written within the boxes. Alternatively fil can be a filling function or nothing.";
];

If[!ValueQ[TableauQ::usage],
  TableauQ::usage = "TableauQ[expr] checks, whether expr is a well defined Young Tableau.";
];

If[!ValueQ[ValidTableauQ::usage],
    ValidTableauQ::usage = "ValidTableauQ[t] gives False, if t is not a Tableaux, or boxes with the same label appear in the same column or the entries do not give a lattice permutation. Otherwise it gives True.";
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

If[!ValueQ[$MaxTableauPrintBoxes::usage],
    $MaxTableauPrintBoxes::usage = "$MaxTableauPrintBoxes is an integer which controls up to what number of points a Tableau is presented graphically in StandardForm.";
];

If[!ValueQ[TableauClear::usage],
    TableauClear::usage = "TableauClear[tab] removes the filling of the given Young Tableau tab.";
];

If[!ValueQ[TableauFirst::usage],
    TableauFirst::usage = "TableauFirst[tab] gives the label of the upper right box of the given Young Tableau tab.";
];

If[!ValueQ[TableauFirstRow::usage],
    TableauFirstRow::usage = "TableauFirstRow[tab] gives the list of labels of the upper row of the given Young Tableau tab.";
];

If[!ValueQ[TableauRest::usage],
    TableauRest::usage = "TableauRest[tab] returns the Tableau tab with the upper right box removed. The result is not necessarily a proper Young Tableau.";
];

If[!ValueQ[TableauRestRows::usage],
    TableauRestRows::usage = "TableauRestRows[tab] returns the Tableau tab with the upper row removed.";
];

If[!ValueQ[TableauAppend::usage],
    TableauAppend::usage = "TableauAppend[tab, row, entry] appends a box with label entry at the end of the row row.";
];

If[!ValueQ[TableauSimplify::usage],
    TableauSimplify::usage = "TableauSimplify[tab, n] removes all columns of length n from the given Tableau tab.";
];

If[!ValueQ[TableauReduce::usage],
    TableauReduce::usage = "TableauReduce[p, d] reduces the TableauProduct assuming degree d.";
];

If[!ValueQ[TableauExpand::usage],
  TableauExpand::usage = "TableauExpand[p] expands the TableauProduct p into a TableauSum.";
];

If[!ValueQ[TableauSum::usage],
    TableauSum::usage = "TableauSum[t1, t2, ...] represents the sum of tableaux t1, t2, etc.";
];

If[!ValueQ[TableauProduct::usage],
    TableauProduct::usage = "TableauProduct[t1, t2, ...] represents the product of tableaux t1, t2, etc.";
];

If[!ValueQ[TableauToMatrix::usage],
    TableauToMatrix::usage = "TableauToMatrix[t] returns a matrix corresponding to the Tableau t.";
];

Begin["`Private`"]


(* Tableau                                     *)
(* =========================================== *)

SetAttributes[Tableau, {HoldFirst}];

SyntaxInformation[Tableau] = {
  (* Tableau must have at least one argument *)
  "ArgumentsPattern" -> {__}
};

(* print visual representation in StandardForm if small enough spec *)
Format[t:Tableau[spec_, ___], StandardForm] :=
Interpretation[
  tableauChart[t],
  t
] /; Total[spec] <= $MaxTableauPrintBoxes

(* always print visual representation in TraditionalForm *)
Format[t:Tableau[spec_, ___], TraditionalForm] :=
Interpretation[
  tableauChart[t],
  t
]

(* automatically fill when supplied with function *)
Tableau[spec_, func_Symbol] := Tableau[spec, func[Tableau[spec]]]
Tableau[spec_, func_Function] := Tableau[spec, func[Tableau[spec]]]

(* messages *)
Tableau::nepty = "The Tableau must be empty."; (* used by filling functions *)


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
        stack = Prepend[filling /. {Empty -> Invisible[1]}, 0],
        stack = Prepend[filling[Tableau[spec]] /. {Empty -> Invisible[1]}, 0]
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
$MaxTableauPrintBoxes = 300;

(* prevent the user from setting invalid values *)
$MaxTableauPrintBoxes /: HoldPattern[(Set | SetDelayed)[$MaxTableauPrintBoxes, x_]] := (
  Message[$MaxTableauPrintBoxes::intOnly];
  $MaxTableauPrintBoxes
) /; (!MemberQ[{Integer, DirectedInfinity}, Head[x]] || Negative[x])

$MaxTableauPrintBoxes::intOnly = "$MaxTableauPrintBoxes must be a non-negative integer or Infinity.";


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


(* ValidTableauQ                               *)
(* =========================================== *)

SetAttributes[ValidTableauQ, {Listable}];

SyntaxInformation[ValidTableauQ] = {
  (* ValidTableauQ must have exactly one argument *)
  "ArgumentsPattern" -> {_}
};

ValidTableauQ[Tableau[spec_]] := TableauQ[Tableau[spec]]

ValidTableauQ[t:Tableau[spec_, fil_?ListQ], groupDegree_, labelset_:Alphabet] := Module[{m,col,p,count,bins,i},
  Catch[
    If[!TableauQ[t], (*Print["no tableau"];*)Throw[False]];

    (** Tableau is invalid if a column is longer than groupDegree **)
    If[Length[spec] > groupDegree, (*Print["too long"];*)Throw[False]];

    (** convert the given tableau to matrix form **)
    m = TableauToMatrix[t];

    (** boxes with the same label must not appear in the same column **)
    If[!AllTrue[Transpose[m] /. (Empty|None) -> Nothing, DuplicateFreeQ], (*Print["same label in col"];*)Throw[False]];

    (** entries must give lattice permutation **)
    (*
      Specifically, when reading from right to left, top to bottom, then at any given
      position in the resulting string there must be more 'a's than 'b's, more 'b's than
      'c's, etc.
    *)
    p = Join[Select[Flatten[Join[Reverse/@m]], (# =!= None && # =!= Empty)&]];

    Which[
      labelset === Alphabet,
      bins = Take[Alphabet[], First@LetterNumber[MaximalBy[fil /. Empty -> Nothing, LetterNumber]]]
      (* ToDo: handle "aa", "ab", etc. cases *),
      labelset === Automatic,
      bins = Sort[DeleteDuplicates[fil]],
      ListQ[labelset],
      bins = labelset,
      True,
      (* ToDo: generate warning message *)
      Throw[$Failed]
    ];
    count = ConstantArray[0, Length[bins]];
    For[i = 1, i <= Length[p], i++,
      count[[First[Flatten[Position[bins,p[[i]]]]]]]++;
      If[Not@OrderedQ[Reverse[count]], (*Print["no lattice"];*)Throw[False]]
    ];

    True
  ]
]


(* TableauToMatrix                             *)
(* =========================================== *)

SyntaxInformation[TableauToMatrix] = {
  (* TableauToMatrix must have exactly one argument *)
  "ArgumentsPattern" -> {_}
};

TableauToMatrix[Tableau[spec_, filling_:{}]] := With[
  {
    fil = PadRight[filling, Total[spec], Empty]
  },
  Table[
    With[{
      a = 1 + Total[spec[[;;(row - 1)]]],
      d = spec[[row]] - 1
    },
      PadRight[fil[[a;;Min[a + d, Length[fil]]]], Max[spec], None]
    ],
    {row, 1, Length[spec]}
  ]
]


(* TableauLetters                              *)
(* =========================================== *)

SyntaxInformation[TableauLetters] = {
  (* TableauLetters must have at least one argument *)
  "ArgumentsPattern" -> {__}
};

TableauLetters[Tableau[spec_], shift_Integer:0] := Flatten[
  Table[
    ConstantArray[
      StringJoin@FromLetterNumber[1 + IntegerDigits[i - 1 + shift, 26, Ceiling@Log[26, 1 + Length[spec] + shift]]],
      spec[[i]]
    ],
    {i, Length[spec]}
  ]
]

TableauLetters[Tableau[_, __], ___] := Message[Tableau::nepty]


(* TableauDistances                            *)
(* =========================================== *)

SyntaxInformation[TableauDistances] = {
  (* TableauDistances must have at least one argument *)
  "ArgumentsPattern" -> {__}
};

TableauDistances[Tableau[spec_, ___],n_:0] := Flatten@Table[
  If[j <= spec[[i]], n-i+j, Nothing],
  {i, Length[spec]},
  {j, spec[[1]]}
]

TableauDistances[Tableau[_, __], ___] := Message[Tableau::nepty]


(* TableauHooks                                *)
(* =========================================== *)

SyntaxInformation[TableauHooks] = {
  (* TableauDistances must have at least one argument *)
  "ArgumentsPattern" -> {__}
};

TableauHooks[Tableau[spec_, ___], n_:0] := Flatten@Table[
  If[j <= spec[[i]], (spec[[i]]-j) + Count[spec[[i;;]], u_ /; (u >= j)], Nothing],
  {i, Length[spec]},
  {j, spec[[1]]}
]

TableauHooks[Tableau[_, __], ___] := Message[Tableau::nepty]


(* TableauDimension                           *)
(* =========================================== *)

SetAttributes[TableauDimension, {Listable}];

SyntaxInformation[TableauDimension] = {
  (* TableauDimension must have at least one argument *)
  "ArgumentsPattern" -> {__}
};

TableauDimension[t:Tableau[spec_, ___], SUGroupDegree_:2] :=
Module[{numBoxes, d, h},
  numBoxes = Total[spec];
  d = TableauDistances[t];
  h = TableauHooks[t];
  Product[(SUGroupDegree + d[[i]]) / h[[i]], {i, 1, numBoxes}]
]


(* TableauClear                                *)
(* =========================================== *)

SetAttributes[TableauClear, {Listable}];

SyntaxInformation[TableauClear] = {
  (* TableauClear must have exactly one argument *)
  "ArgumentsPattern" -> {_}
};

TableauClear[Tableau[spec_, fil_]] := Tableau[spec];
TableauClear[Tableau[spec_]] := Tableau[spec];

(* automatically thread of TableauSum and TableauProduct *)
TableauClear[s:TableauSum[terms__]] := TableauClear /@ s;
TableauClear[p:TableauProduct[terms__]] := TableauClear /@ p;


(* TableauFirst                                *)
(* =========================================== *)

SetAttributes[TableauFirst, {Listable}];

SyntaxInformation[TableauFirst] = {
  (* TableauFirst must have exactly one argument *)
  "ArgumentsPattern" -> {_}
};

TableauFirst[Tableau[spec_List, fil_List]] :=
If[
  Length[spec] > 0,
  If[Length[fil] >= First[spec],
    (* return upper right box label *)
    fil[[First[spec]]],
    (* no label specified for upper right box *)
    Message[General::partw, First[spec], fil]; $Failed
  ],
  (* not enough boxes *)
  Message[General::partw, 1, spec]; $Failed
]

TableauFirst[Tableau[spec_List]] := TableauFirst[Tableau[spec, {}]]


(* TableauFirstRow                             *)
(* =========================================== *)

SetAttributes[TableauFirstRow, {Listable}];

SyntaxInformation[TableauFirstRow] = {
(* TableauFirstRow must have exactly one argument *)
  "ArgumentsPattern" -> {_}
};

TableauFirstRow[Tableau[spec_List, fil_List]] :=
    If[
      Length[spec] > 0,
      If[Length[fil] >= First[spec],
      (* return upper right box label *)
        Take[fil, First[spec]],
      (* no label specified for upper row *)
        Message[General::partw, First[spec], fil]; $Failed
      ],
    (* not enough boxes *)
      Message[General::partw, 1, spec]; $Failed
    ]

TableauFirstRow[Tableau[spec_List]] := TableauFirstRow[Tableau[spec, {}]]


(* TableauRest                                 *)
(* =========================================== *)

SyntaxInformation[TableauRest] = {
  (* TableauRest must have exactly one argument *)
  "ArgumentsPattern" -> {_}
};

TableauRest[Tableau[spec_List, fil_List]] := If[First[spec] == 1,
  (* there is only one element in the top row *)
  Tableau[
    Evaluate@Drop[spec, 1],
    If[Length[fil] > 1, Drop[fil, 1], {}]
  ],
  (* the top row contains more than one element *)
  Tableau[
    (* shorten the first row by one and keep the lengths of the others *)
    Evaluate[{First[spec]-1}~Join~Rest[spec]],
    (* drop the last filling element, if it exists *)
    If[Length[fil] >= First[spec],
      Drop[fil, {First[spec]}],
      fil
    ]
  ]
]

TableauRest[Tableau[spec_List]] := TableauRest[Tableau[spec, {}]]

TableauRest[Tableau[{}]] = None;
TableauRest[Tableau[{},{}]] = None;
TableauRest[None] = None;


(* TableauRestRows                              *)
(* =========================================== *)

SyntaxInformation[TableauRestRows] = {
  (* TableauRest must have exactly one argument *)
  "ArgumentsPattern" -> {_}
};

TableauRestRows[Tableau[spec_List, fil_List]] := Tableau[
  Evaluate@Drop[spec, 1],
  If[Length[fil] > spec[[1]], Drop[fil, spec[[1]]], {}]
]

TableauRestRows[Tableau[spec_List]] := TableauRestRows[Tableau[spec, {}]]

TableauRestRows[Tableau[{}]] = None;
TableauRestRows[Tableau[{},{}]] = None;
TableauRestRows[None] = None;


(* TableauAppend                               *)
(* =========================================== *)

SyntaxInformation[TableauAppend] = {
  (* TableauAppend has at least two arguments *)
  "ArgumentsPattern" -> {_, __}
};

TableauAppend[Tableau[spec_List, fil_List], row_, entry_:Empty] :=
If[row <= Length[spec],
  (* append to one of the existing rows *)
  Tableau[
    Evaluate@ReplacePart[spec, row -> (spec[[row]] + 1)],
    Insert[PadRight[fil, Total[spec], Empty], entry, 1 + Total[spec[[;;row]]]]
  ] /. replaceEmptyFillings
  ,
  If[row > Length[spec] + 1,
    Message[TableauAppend::tl, row, Length[spec]];
    $Failed
    ,
    (* append below the Tableaux (add a new row) *)
    Tableau[
      Evaluate@Append[spec, 1],
      Append[PadRight[fil, Total[spec], Empty], entry]
    ] /. replaceEmptyFillings
  ]
] /; !ListQ[entry]

(* automatically append lists of elements in one row *)
TableauAppend[tab_Tableau, row_, entry_List] := Fold[TableauAppend[#1, row, #2] &, tab, entry]

(* use this rule to simplify Tableau expressions by removing empty fillings (in the end) *)
replaceEmptyFillings = {
  (* replace Tableau[spec, {}] or Tableau[spec, {None, None, ...}] with Tableau[spec] *)
  Tableau[s_, {Empty...}] :> Tableau[s],
  (* drop final None entries *)
  Tableau[s_, {f__, Empty..}] :> Tableau[s, {f}]
}

(* deal with un-filled tableaux *)
TableauAppend[Tableau[spec_], rest__] := TableauAppend[Tableau[spec, {}], rest]

TableauAppend::tl = "Cannot append in row `1` when Tableau has only `2` rows.";



(* TableauSimplify                             *)
(* =========================================== *)

SyntaxInformation[TableauSimplify] = {
  (* TableauSimplify must have exactly two arguments *)
  "ArgumentsPattern" -> {_, _}
};

TableauSimplify[Tableau[spec_List, filling_List], groupDegree_Integer] := Module[{fil, newSpec, newFil},
  Catch[
    If[spec == {}, Throw[Tableau[{}]]];
    fil = PadRight[filling, Total[spec], Empty];

    (* delete columns with groupDegree boxes *)
    If[Length[spec] == groupDegree,
      newSpec = (#-1)& /@ spec;
      While[newSpec != {} && Last[newSpec] <= 0, newSpec = Most[newSpec]];
      newFil = Delete[fil, List /@ Prepend[Table[Fold[Plus, 1, spec[[;;i]]], {i, Length[spec] - 1}], 1]];
      TableauSimplify[Tableau[Evaluate@newSpec, newFil], groupDegree]
      ,
      newSpec = spec;
      (*While[newSpec != {} && Last[newSpec] <= 0, newSpec = Most[newSpec]];*)
      Tableau[Evaluate@newSpec, fil]
    ]
  ] /. replaceEmptyFillings
]

TableauSimplify[Tableau[spec_], groupDegree_Integer] := TableauSimplify[Tableau[spec, {}], groupDegree]

TableauSimplify[tabs_List, groupDegree_] := TableauSimplify[#, groupDegree]& /@ tabs

TableauSimplify[tabs_TableauSum, groupDegree_] := TableauSimplify[#, groupDegree]& /@ tabs


(* TableauExpand                               *)
(* =========================================== *)

TableauExpand[TableauProduct[a___, sum_TableauSum, b___]] := TableauSum@@Table[
  TableauProduct[a, sum[[i]], b],
  {i, Length[sum]}
]

TableauExpand[TableauSum[a___, prod_TableauProduct, b___]] := TableauSum[a, TableauExpand[prod], b]

(* trivial cases *)
TableauExpand[expr_TableauSum] := expr /; FreeQ[expr, TableauProduct];
TableauExpand[expr_TableauProduct] := expr /; FreeQ[expr, TableauSum];


(* TableauReduce                               *)
(* =========================================== *)

(* the algorithm is described, for example in *)
(* http://physik.uni-graz.at/~gxe/2013-hadron-physics/hadron-app-3.pdf *)

Options[TableauReduce] = {StepMonitor :> Null};

SyntaxInformation[TableauReduce] = {
  (* TableauReduce must have exactly two arguments *)
  "ArgumentsPattern" -> {_, _, OptionsPattern[]}
};

TableauReduce[TableauProduct[t1_Tableau, t2_Tableau], deg_Integer, OptionsPattern[]] := tabReduce[
  TableauClear[t1],
  Tableau[Evaluate[t2[[1]]], TableauLetters],
  deg,
  OptionValue[StepMonitor]
]

TableauReduce[TableauProduct[t1_Tableau, t2_Tableau, rest__], deg_Integer, options:OptionsPattern[]] := Module[{sum, s},
  sum = TableauReduce[TableauProduct[t1, t2], deg, options];
  TableauSum@@Table[
    TableauReduce[TableauProduct[s, rest], deg, options],
    {s, List@@sum}
  ]
]

ClearAll[tabReduce];

tabReduce[tab1_Tableau, tab2_Tableau, deg_, monitor_] := Catch[
  Module[{(*tab1, tab2, *)entry, rest, res, t},
    (*tab1 = TableauClear[t1];*)
    (*If[t2 === TableauClear[t2],
      tab2 = Tableau[Evaluate[t2[[1]]], TableauLetters],
      tab2 = t2
    ];*)
    (*tab2 = Tableau[Evaluate[t2[[1]]], TableauLetters];*)

    (* the product of x with 1 is just x *)
    Which[
      MatchQ[tab1, Tableau[{}, ___]], Throw[tab2],
      MatchQ[tab2, Tableau[{}, ___]], Throw[tab1]
    ];

    (* take upper right box from tab2 *)
    entry = TableauFirst[tab2];

    (* the remaining tableau is *)
    rest = TableauRest[tab2];

    (* add entry to tab1 in all possible ways *)
    res = Table[
      TableauAppend[tab1, row, entry],
      {row, 1 + Length[tab1[[1]]]} (* for each row in tab1 *)
    ];
    (* filter out arrangements that are no Young Tableaux *)
    res = Select[res, TableauQ];
    (*monitor["generate combinations", TableauProduct[TableauSum@@res, rest]];*)
    (* filter out invalid tableaux *)
    res = Select[res, ValidTableauQ[#, deg]&];
    (*monitor["delete invalid", TableauProduct[TableauSum@@res, rest]];*)
    (* delete columns of length deg *)
    res = TableauSimplify[res, deg];
    (*monitor["remove columns", TableauProduct[TableauSum@@res, rest]];*)
    (* delete duplicate tableaux *)
    res = DeleteDuplicates@res;
    (*monitor["delete duplicates", TableauProduct[TableauSum@@res, rest]];*)

    monitor[" simplified ", TableauProduct[TableauSum@@res, rest]];

    (* for each term, reduce the product with the rest *)
    DeleteDuplicates[
      TableauSum@@Table[
        tabReduce[t, rest, deg, monitor[Row[{t, ">", #1}], #2]&],
        {t, res}
      ]
    ]
  ]
]

(* TableauSum                                  *)
(* =========================================== *)

(* print visual representation in TraditionalForm *)
Format[t:TableauSum[_, __], TraditionalForm] :=
  Interpretation[
    Row@Riffle[List@@t, " \[CirclePlus] "],
    t
  ]

TableauSum[x_Tableau] := x

(* Note: the Flat attribute must be set after setting DownValues *)
(* https://mathematica.stackexchange.com/q/5067/35390 *)
SetAttributes[TableauSum, {
  Flat, (* associative *)
  OneIdentity, (* the sum of one element is the element *)
  Orderless (* commutative *)
}];


(* TableauProduct                              *)
(* =========================================== *)

(* print visual representation in TraditionalForm *)
Format[t:TableauProduct[terms__], TraditionalForm] :=
  Interpretation[
    Row@Flatten@Riffle[If[Head[#] === TableauSum, {Style["(", Large], #, Style[")", Large]}, #]& /@ List@@t, " \[CircleTimes] "],
    t
  ]

TableauProduct[x_Tableau] := x

(* Note: the Flat attribute must be set after setting DownValues *)
(* https://mathematica.stackexchange.com/q/5067/35390 *)
SetAttributes[TableauProduct, {
  Flat, (* associative *)
  OneIdentity (* the product of one element is the element *)
}];

End[] (* `Private` *)

EndPackage[]