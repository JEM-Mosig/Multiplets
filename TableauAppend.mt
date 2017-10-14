(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauAppend.mt"]

(* valid arguments *)

VerificationTest[
  TableauAppend[Tableau[{2, 1}, {5, 6, 7}], 1, "x"]
  ,
  Tableau[{3, 1}, {5, 6, "x", 7}]
]

VerificationTest[
  TableauAppend[Tableau[{2, 1}], 1, "x"]
  ,
  Tableau[{3, 1}, {None, None, "x"}]
]

VerificationTest[
  TableauAppend[Tableau[{}], 1]
  ,
  Tableau[{1}]
]

VerificationTest[
  TableauAppend[Tableau[{2, 1}], 1]
  ,
  Tableau[{3, 1}]
]

VerificationTest[
  TableauAppend[Tableau[{2, 1}, {1, 2, 3}], 1]
  ,
  Tableau[{3, 1}, {1, 2, None, 3}]
]

VerificationTest[
  TableauAppend[Tableau[{2, 1}], 2]
  ,
  Tableau[{2, 2}]
]

VerificationTest[
  TableauAppend[Tableau[{2, 1}], 3]
  ,
  Tableau[{2, 1, 1}]
]

(* invalid arguments *)

VerificationTest[
  TableauAppend[Tableau[{2, 1}], 4]
  ,
  $Failed
  ,
  {TableauAppend::tl}
]


EndTestSection[]
