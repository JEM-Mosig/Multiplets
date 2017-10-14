(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauRest.mt"]

VerificationTest[
  TableauRest[
    Tableau[{4, 2, 1}, {1, 2, 3, 4, 5, 6, 7}]
  ]
  ,
  Tableau[{3, 2, 1}, {1, 2, 3, 5, 6, 7}]
]

VerificationTest[
  TableauRest[Tableau[{}]]
  ,
  None
]

VerificationTest[
  TableauRest[Tableau[{},{}]]
  ,
  None
]

VerificationTest[
  TableauRest[None]
  ,
  None
]

VerificationTest[
  TableauRest[
    Tableau[{2, 2, 1}, {1, 2, 5, 6, 7}]
  ]
  ,
  Tableau[{1, 2, 1}, {1, 5, 6, 7}]
]

EndTestSection[]
