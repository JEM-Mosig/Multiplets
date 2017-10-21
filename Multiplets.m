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
    MultipletReduce::usage = "MultipletReduce[p] converts the MultipletProduct p into a MultipletSum of irreducible representations.";
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
  StandardForm -> "Dimension",
  TraditionalForm -> "Dimension"
};

SyntaxInformation[Multiplet] = {
  "ArgumentsPattern" -> {_, OptionsPattern[]}
};

(* print visual representation in StandardForm if small enough dimension in SU(2) *)
Format[mplet:Multiplet[{a_Integer}, options:OptionsPattern[Multiplet]], StandardForm] :=
  su2formatting[mplet, a, OptionValue[{options, Options[Multiplet]}, StandardForm]] /;
    And[
      Nand[OptionValue[{options, Options[Multiplet]}, StandardForm] === "Diagram", a + 1 > $MaxMultipletPrintDimensionSU2],
      OptionValue[{options, Options[Multiplet]}, StandardForm] =!= "Plain"
    ]

(* always use dimension form in TraditionalForm *)
Format[mplet:Multiplet[{a_Integer}, options:OptionsPattern[Multiplet]], TraditionalForm] :=
    su2formatting[mplet, a, OptionValue[{options, Options[Multiplet]}, TraditionalForm]] /;
        OptionValue[{options, Options[Multiplet]}, TraditionalForm] =!= "Plain"

(* print visual representation in StandardForm if small enough dimension in SU(3) *)
Format[mplet:Multiplet[{a_Integer, b_Integer}, options:OptionsPattern[Multiplet]], StandardForm] :=
  su3formatting[mplet, a, b, OptionValue[{options, Options[Multiplet]}, StandardForm]] /;
    And[
      Nand[OptionValue[{options, Options[Multiplet]}, StandardForm] === "Diagram", (a+1)(b+1)(a+b+2) > 2 $MaxMultipletPrintDimensionSU3],
      OptionValue[{options, Options[Multiplet]}, StandardForm] =!= "Plain"
    ]

(* always use dimension form in TraditionalForm *)
Format[mplet:Multiplet[{a_Integer, b_Integer}, options:OptionsPattern[Multiplet]], TraditionalForm] :=
  su3formatting[mplet, a, b, OptionValue[{options, Options[Multiplet]}, TraditionalForm]] /;
      OptionValue[{options, Options[Multiplet]}, TraditionalForm] =!= "Plain"

ClearAll[su2formatting];
su2formatting[mplet_, a_, form_] := Switch[form,
  "Diagram",
  Interpretation[
    Deploy@Tooltip[
      multipletChart[mplet],
      Column[{
        "SU(2) Multiplet ("<>ToString[a]<>")",
        "Dimension: "<>ToString[a+1]
      }]
    ],
    mplet
  ],
  "Dimension",
  Interpretation[
    Deploy@Tooltip[
      Style[a + 1, Bold, ColorData[112, 2] (* blue *), Large],
      Column[{
        "SU(2) Multiplet ("<>ToString[a]<>")",
        "Dimension: "<>ToString[a+1]
      }]
    ],
    mplet
  ],
  _,
  Message[Multiplet::bdfmt, OptionValue[form, OutputForm]]; Abort[]
]

ClearAll[su3formatting];
su3formatting[mplet_, a_, b_, form_] := Switch[form,
  "Diagram",
  Interpretation[
    Deploy@Tooltip[
      multipletChart[mplet],
      Column[{
        "SU(3) Multiplet ("<>ToString[a]<>","<>ToString[b]<>")",
        "Dimension: "<>ToString[(a+1)(b+1)(a+b+2)/2]
      }]
    ],
    mplet
  ],
  "Dimension",
  Interpretation[
    Deploy@Tooltip[
      With[{dim = (a+1)(b+1)(a+b+2)/2},
        If[a >= b,
          Style[dim, Bold, Large, ColorData[112, 2] (* blue *)],
          Style[OverBar@dim, Bold, Large, ColorData[112, 2] (* blue *)]
        ]
      ],
      Column[{"SU(3) Multiplet ("<>ToString[a]<>","<>ToString[b]<>")", "Dimension: "<>ToString[(a+1)(b+1)(a+b+2)/2]}]
    ],
    mplet
  ],
  _,
  Message[Multiplet::bdfmt, OptionValue[form, OutputForm]]; Abort[]
]

Multiplet::bdfmt = "`1` must be one of \"Diagram\", \"Dimension\", or \"Plain\".";


(* $MaxMultipletPrintDimensionSU~              *)
(* =========================================== *)

(* set the default value *)
$MaxMultipletPrintDimensionSU2 = 10;
$MaxMultipletPrintDimensionSU3 = 100;

(* prevent the user from setting invalid values *)
$MaxMultipletPrintDimensionSU2 /: HoldPattern[(Set | SetDelayed)[$MaxMultipletPrintDimensionSU2, x_]] := (
  Message[$MaxMultipletPrintDimensionSU2::intOnly];
  $MaxMultipletPrintDimensionSU2
) /; (!MemberQ[{Integer, DirectedInfinity}, Head[x]] || Negative[x])

$MaxMultipletPrintDimensionSU2::intOnly = "$MaxMultipletPrintDimensionSU2 must be a non-negative integer or Infinity.";

$MaxMultipletPrintDimensionSU3 /: HoldPattern[(Set | SetDelayed)[$MaxMultipletPrintDimensionSU3, x_]] := (
  Message[$MaxMultipletPrintDimensionSU3::intOnly];
  $MaxMultipletPrintDimensionSU3
) /; (!MemberQ[{Integer, DirectedInfinity}, Head[x]] || Negative[x])

$MaxMultipletPrintDimensionSU3::intOnly = "$MaxMultipletPrintDimensionSU3 must be a non-negative integer or Infinity.";


(* multipletChart (PRIVATE)                    *)
(* =========================================== *)

multipletChart[Multiplet[{a_Integer,  b_Integer}, ___]] := If[a == 0 && b == 0,
  (* draw just one dot (necessary to avoid oversize dot) *)
  Graphics[{Black, Disk[{0, 0}], Transparent, Rectangle[{-10, -1}, {10, 1}]}, ImageSize->Tiny]
  , 
  (* draw diagram *)
  Module[{Tup, Uup, Vup, Tdn, Udn, Vdn}, 
    (* define raising and lowering operators *)
    Tup[{x_, y_}]:={x+1, y};
    Uup[{x_, y_}, f_:1]:={x-f/2, y+f};
    Vup[{x_, y_}]:={x+1/2, y+1};
    Tdn[{x_, y_}]:={x-1, y};
    Udn[{x_, y_}]:={x+1/2, y-1};
    Vdn[{x_, y_}]:={x-1/2, y-1};

    (* draw SU(3) multiplet diagram *)
    Graphics[
      Translate[
        {
          (* construct boundary *)
          Thick,  Black, 
          Line[
            ComposeList[Join[
              ConstantArray[Tup, a], 
              ConstantArray[Udn, b], 
              ConstantArray[Vdn, a], 
              ConstantArray[Tdn, b], 
              ConstantArray[Uup, a], 
              ConstantArray[Vup, b]
            ], {0, 0}]
          ], 
          (* fill with states inside boundary *)
          ColorData[112, 2] (* blue *),
          Disk[#,  0.1]& /@ ComposeList[
            (* 'Rest' undoes the first Udn move *)
            Rest@Flatten@Table[{
                {Udn},  (* next layer (go inside) *)
                ConstantArray[Tup, Max[a-n, 0]], 
                ConstantArray[Udn, Max[b-n, 0]], 
                ConstantArray[Vdn, Max[a-n, 0]], 
                ConstantArray[Tdn, Max[b-n, 0]], 
                ConstantArray[Uup, Max[a-n, 0]], 
                ConstantArray[Vup, Max[b-n, 0]]
              }, 
              {n, 0, Max[{a, b}]}
            ], 
            {0, 0}
          ]
        },  
        {Uup[{0, 0}, Max[{a, b}]]} (* translate origin *)
      ], 
      ImageSize -> {128, 128},
      AspectRatio -> 1
    ]
  ]
]

multipletChart[Multiplet[{a_Integer}, ___]] := Graphics[{
    Thick, Black,
    Line[{{0, 0}, {1, 0}}],
    ColorData[112, 2],
    Disk[{#, 0}, 0.05] & /@ Subdivide[0, 1, a]
  },
  ImageSize -> {128, 32}
]

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

MultipletDimension[m:Multiplet[{a_Integer}, ___]] := a + 1

MultipletDimension[m:Multiplet[{a_Integer, b_Integer}, ___]] := Quotient[(a+1)(b+1)(a+b+2), 2]


End[] (* `Private` *)

EndPackage[]