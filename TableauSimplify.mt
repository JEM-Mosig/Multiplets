(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauSimplify.mt"]

VerificationTest[
  TableauSimplify[Tableau[{2, 1}], 2]
  ,
  Tableau[{1}]
]

VerificationTest[
  Table[
    TableauSimplify[Tableau[{5, 3, 3, 1, 1}], n],
    {n, 6, 1, -1}
  ]
  ,
  {
    Tableau[{5, 3, 3, 1, 1}],
    Tableau[{4, 2, 2}],
    Tableau[{4, 2, 2}],
    Tableau[{2}],
    Tableau[{2}],
    Tableau[{}]
  }
]

EndTestSection[]
