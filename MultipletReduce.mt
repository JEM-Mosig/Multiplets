(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["MultipletReduce.mt"]

(* test against a few examples from *)
(* "The Lie Algebras su(N): An Introduction" *)
(* by Walter Pfeifer, ISBN: 376432418X, 9783764324186 *)

VerificationTest[
  MultipletReduce[
    MultipletProduct[Multiplet[{1,0}], Multiplet[{0,1}]]
  ]
  ,
  MultipletSum[Multiplet[{1,1}], Multiplet[{0,0}]]
]

VerificationTest[
  MultipletReduce[
    MultipletProduct[Multiplet[{1,0}], Multiplet[{1,0}], Multiplet[{1,0}]]
  ]
  ,
  MultipletSum[Multiplet[{3,0}], Multiplet[{1,1}], Multiplet[{1,1}], Multiplet[{0,0}]]
]

VerificationTest[
  MultipletReduce[
    MultipletProduct[Multiplet[{1,0}], Multiplet[{1,0}]]
  ]
  ,
  MultipletSum[Multiplet[{2,0}], Multiplet[{0,1}]]
]

VerificationTest[
  MultipletReduce[
    MultipletProduct[Multiplet[{1,1}], Multiplet[{1,0}]]
  ]
  ,
  MultipletSum[Multiplet[{2,1}], Multiplet[{0,2}], Multiplet[{1,0}]]
]

VerificationTest[
  MultipletReduce[
    MultipletProduct[Multiplet[{2,1}], Multiplet[{1,2}]]
  ]
  ,
  MultipletSum[
    Multiplet[{3,3}],
    Multiplet[{4,1}],
    Multiplet[{1,4}],
    Multiplet[{2,2}], Multiplet[{2,2}],
    Multiplet[{0,3}], Multiplet[{0,3}],
    Multiplet[{1,1}], Multiplet[{1,1}],
    Multiplet[{0,0}]
  ]
]


(* some examples which caused trouble before *)

VerificationTest[
  MultipletReduce[MultipletProduct[Multiplet[{1}], Multiplet[{1}]]]
  ,
  MultipletSum[Multiplet[{0}], Multiplet[{2}]]
]

VerificationTest[
  Total@MultipletDimension[List@@MultipletReduce[MultipletProduct[Multiplet[{1}], Multiplet[{3}]]], 2]
  ,
  8
]

EndTestSection[]
