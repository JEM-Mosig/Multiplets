(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["MultipletDimension.mt"]

VerificationTest[
  MultipletDimension[Multiplet[{3}]]
  ,
  4
]

VerificationTest[
  MultipletDimension[Multiplet[{1,2,3,4}]]
  ,
  198450
]

EndTestSection[]
