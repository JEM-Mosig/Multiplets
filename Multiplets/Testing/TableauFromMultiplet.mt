(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauFromMultiplet.mt"]

VerificationTest[
  TableauFromMultiplet[Multiplet[{1, 2}]]
  ,
  Tableau[{3, 2}]
]

VerificationTest[
  TableauFromMultiplet[Multiplet[{1, 2}], TableauHooks]
  ,
  Tableau[{3, 2}, {4, 3, 1, 2, 1}]
]

EndTestSection[]
