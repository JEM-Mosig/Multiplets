(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauClear.mt"]

VerificationTest[
  TableauClear[Tableau[{}]]
  ,
  Tableau[{}]
]

VerificationTest[
  TableauClear[Tableau[{4, 2, 2, 1}, {1, 2, 3, 4, 5, 6, 7, 8, 9}]]
  ,
  Tableau[{4, 2, 2, 1}]
]

EndTestSection[]
