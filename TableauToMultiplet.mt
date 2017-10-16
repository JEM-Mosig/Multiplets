(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauToMultiplet.mt"]

VerificationTest[
  TableauToMultiplet[Tableau[{3,2}]]
  ,
  Multiplet[{1,2}]
]

EndTestSection[]
