(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauQ.mt"]

(* Tableaux that are NOT proper YoungTableaux *)

VerificationTest[
  TableauQ@Tableau[{2, 1}, {a, b, c, d}]
  ,
  False
]

VerificationTest[
  TableauQ@Tableau[{3, 5, 1}]
  ,
  False
]

(* Tableaux that ARE proper YoungTableaux *)

VerificationTest[
  TableauQ@Tableau[{2, 1}, {a, b, c}]
  ,
  True
]

VerificationTest[
  TableauQ@Tableau[{5, 4, 2, 2} / {2, 1}]
  ,
  True
]

EndTestSection[]
