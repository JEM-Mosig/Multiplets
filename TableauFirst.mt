(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauFirst.mt"]

(* valid inputs *)

VerificationTest[
  TableauFirst[
    Tableau[{4, 2, 1}, {1, 2, 3, 4, 5, 6, 7}]
  ]
  ,
  4
]

(* invalid inputs *)

VerificationTest[
  TableauFirst[
    Tableau[{}]
  ]
  ,
  $Failed
  ,
  {General::partw}
]

VerificationTest[
  TableauFirst[
    Tableau[{2, 1}, {1}]
  ]
  ,
  $Failed
  ,
  {General::partw}
]

EndTestSection[]
