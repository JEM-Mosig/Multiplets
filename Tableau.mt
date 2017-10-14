(* Mathematica Test File    *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginTestSection["Tableau.mt"]

(* test $MaxTableauPrintPoints *)
(* ====================================== *)

VerificationTest[
  $MaxTableauPrintPoints = -1
  ,
  $MaxTableauPrintPoints
  ,
  {$MaxTableauPrintPoints::intOnly}
]

VerificationTest[
  $MaxTableauPrintPoints = 1.1
  ,
  $MaxTableauPrintPoints
  ,
  {$MaxTableauPrintPoints::intOnly}
]

VerificationTest[
  Module[{val = $MaxTableauPrintPoints},
    $MaxTableauPrintPoints = Infinity;
    $MaxTableauPrintPoints = val;
  ]
  ,
  Null
  ,
  {}
]

VerificationTest[
  Module[{val = $MaxTableauPrintPoints},
    $MaxTableauPrintPoints = 23;
    $MaxTableauPrintPoints = val;
  ]
  ,
  Null
  ,
  {}
]

(* test automatic filling *)
(* ====================== *)

(* TableauLetters *)

VerificationTest[
  Tableau[{1}, TableauLetters]
  ,
  Tableau[{1}, {"a"}]
]

VerificationTest[
  Tableau[{2}, TableauLetters]
  ,
  Tableau[{2}, {"a", "a"}]
]

VerificationTest[
  Tableau[{4, 3, 2, 1}, TableauLetters]
  ,
  Tableau[{4, 3, 2, 1}, {"a", "a", "a", "a", "b", "b", "b", "c", "c", "d"}]
]

VerificationTest[
  Tableau[Evaluate@ConstantArray[1, 30], TableauLetters]
  ,
  Tableau[{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {
    "aa", "ab", "ac", "ad", "ae", "af", "ag", "ah", "ai", "aj", "ak",
    "al", "am", "an", "ao", "ap", "aq", "ar", "as", "at", "au", "av",
    "aw", "ax", "ay", "az", "ba", "bb", "bc", "bd"}
  ]
]

(* TableauDistances *)

VerificationTest[
  Tableau[{1}, TableauDistances]
  ,
  Tableau[{1}, {0}]
]

VerificationTest[
  Tableau[{4, 3, 2, 1}, TableauDistances]
  ,
  Tableau[{4, 3, 2, 1}, {0, 1, 2, 3, -1, 0, 1, -2, -1, -3}]
]

(* TableauHook *)

VerificationTest[
  Tableau[{4, 3, 2, 1}, TableauHooks]
  ,
  Tableau[{4, 3, 2, 1}, {7, 5, 3, 1, 5, 3, 1, 3, 1, 1}]
]

EndTestSection[]
