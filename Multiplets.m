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

BeginPackage["Multiplets`"]

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

Begin["`Private`"]

End[] (* `Private` *)

EndPackage[]