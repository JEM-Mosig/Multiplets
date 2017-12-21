(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["MultipletQ.mt"]

VerificationTest[
  MultipletQ[{1, 2, 3}]
  ,
  False
]

VerificationTest[
  AllTrue[{Multiplet[{3}], Multiplet[{0, 2}], Multiplet[{2, 0}]}, MultipletQ]
  ,
  True
]

EndTestSection[]
