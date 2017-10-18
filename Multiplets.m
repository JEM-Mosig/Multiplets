(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Multiplets *)
(* :Context: Multiplets` *)
(* :Author: jem-mosig@protonmail.com *)
(* :Date: 2017-10-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.1 *)
(* :Copyright: (c) 2017 jem-mosig@protonmail.com *)
(* :Keywords: multiplet, group theory, quantum physics *)
(* :Discussion: *)

BeginPackage["Multiplets`", {"YoungTableaux`"}]

If[!ValueQ[Multiplet::usage],
    Multiplet::usage = "Multiplet[spec] is a wrapper for a general SU(n) Multiplet.";
];

If[!ValueQ[MultipletQ::usage],
    MultipletQ::usage = "MultipletQ[expr] returns True if expr is a well defined Multiplet, and False otherwise.";
];

If[!ValueQ[MultipletSum::usage],
    MultipletSum::usage = "MultipletSum[m1, m2] represents the sum of the multiplets m1 and m2.";
];

If[!ValueQ[MultipletProduct::usage],
    MultipletProduct::usage = "MultipletProduct[m1, m2] represents the product of the multiplets m1 and m2.";
];

If[!ValueQ[MultipletReduce::usage],
    MultipletReduce::usage = "MultipletReduce[p] convertes the MultipletProduct p into a MultipletSum of irreducible representations.";
];

If[!ValueQ[MultipletDimension::usage],
  MultipletDimension::usage = "MultipletDimension[m] gives the dimension of the multiplet m.";
];

If[!ValueQ[$MaxMultipletPrintDimensionSU2::usage],
    $MaxMultipletPrintDimensionSU2::usage = "$MaxMultipletPrintDimensionSU2 is an integer which controls up to what dimension a SU(2) Multiplet is presented graphically in StandardForm.";
];

If[!ValueQ[$MaxMultipletPrintDimensionSU3::usage],
  $MaxMultipletPrintDimensionSU3::usage = "$MaxMultipletPrintDimensionSU3 is an integer which controls up to what dimension a SU(3) Multiplet is presented graphically in StandardForm.";
];

If[!ValueQ[TableauFromMultiplet::usage],
    TableauFromMultiplet::usage = "TableauFromMultiplet[m] returns the Tableau which corresponds to the Multiplet m.";
];

If[!ValueQ[TableauToMultiplet::usage],
    TableauToMultiplet::usage = "TableauToMultiplet[t, n] returns the SU(n) Multiplet which corresponds to the Tableau t. By default, n = 3.";
];

Begin["`Private`"]


(* Multiplet                                   *)
(* =========================================== *)

Options[Multiplet] = {
  (* control how the multiplet should be represented *)
  (* "Plain", "Diagram", "Dimension" *)
  OutputForm -> "Plain"
};

SyntaxInformation[Multiplet] = {
  "ArgumentsPattern" -> {_, OptionsPattern[]}
};


(* MultipletQ                                  *)
(* =========================================== *)

SyntaxInformation[MultipletQ] = {
  (* MultipletQ must be called with a single argument *)
  "ArgumentsPattern" -> {_}
};

MultipletQ[expr_] := TrueQ@Quiet@And[
  Head[expr] === Multiplet,
  Length[expr] == 1,
  Head[expr[[1]]] === List,
  VectorQ[expr[[1]]],
  AllTrue[expr[[1]], IntegerQ],
  AllTrue[expr[[1]], NonNegative]
]


(* TableauToMultiplet                          *)
(* =========================================== *)

SyntaxInformation[TableauToMultiplet] = {
  (* TableauToMultiplet must be called with at least one argument *)
  "ArgumentsPattern" -> {_, _}
};

TableauToMultiplet[Tableau[spec_, ___], groupDegree_:3] := Multiplet[
  PadRight[-Differences[Append[spec, 0]], groupDegree - 1, 0]
]


(* TableauFromMultiplet                        *)
(* =========================================== *)

TableauFromMultiplet[Multiplet[mplet_List, ___]] := Tableau[
  Evaluate@Select[Reverse@Accumulate@Reverse@mplet, Positive]
]

TableauFromMultiplet[Multiplet[mplet_List, ___], fil_] := Tableau[
  Evaluate@Select[Reverse@Accumulate@Reverse@mplet, Positive],
  fil
]


(* MultipletDimension                          *)
(* =========================================== *)

SetAttributes[MultipletDimension, {Listable}];

SyntaxInformation[MultipletDimension] = {
  (* MultipletDimension must be called with a single argument *)
  "ArgumentsPattern" -> {_}
};

MultipletDimension[m:Multiplet[spec_, ___]] := TableauDimension[
  TableauFromMultiplet[m], Length[spec] + 1
]


End[] (* `Private` *)

EndPackage[]