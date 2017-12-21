(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauDimension.mt"]

VerificationTest[
  TableauDimension@Tableau[{2, 1}]
  ,
  2
]

VerificationTest[
  TableauDimension[Tableau[{2, 1}], 3]
  ,
  8
]

VerificationTest[
  TableauDimension[Tableau[{2, 1}], 12]
  ,
  572
]

EndTestSection[]
