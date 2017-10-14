(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["TableauToMatrix.mt"]

VerificationTest[
  TableauToMatrix[Tableau[{5, 2, 1}]]
  ,
  {
    {Empty, Empty, Empty, Empty, Empty},
    {Empty, Empty, None, None, None},
    {Empty, None, None, None, None}
  }
]

VerificationTest[
  TableauToMatrix[Tableau[{4, 2, 1}, {1, 2, 3, 4, 5, 6, 7}]]
  ,
  {
    {1, 2, 3, 4},
    {5, 6, None, None},
    {7, None, None, None}
  }
]

EndTestSection[]
