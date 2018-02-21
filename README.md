# Multiplets
Mathematica package for SU(n) multiplets and Young tableaux.

## Installation instructions (for development)
The following instructions allow you to setup Mathematica such that you can load the *Multiplets* paclet with `` << Multiplets` `` in any notebook, without installing it permanently.

Download the *Multiplets* directory into a directory of your choice, say `c:\myMMAPaclets\`. Within Mathematica, type 

```mathematica
NotebookOpen@FileNameJoin[{$UserBaseDirectory, "Kernel", "init.m"}]
```

This opens the *init.m* script where you can specify commands that are executed automatically whenever the Mathematica kernel starts.
Add

```mathematica
PacletDirectoryAdd["c:\\myMMAPaclets\\"]
```

at the end of the *init.m* script, save, and close the script.
Finally, quit the kernel (Evaluation -> Quit Kernel -> Local). 

## Basic examples
Open a new Mathematica notebook and type

```mathematica
<< Multiplets`
```

to load the paclet. You can now display the list of newly defined symbols with `` ?Multiplets`* ``.

Any SU(n) multiplet can be represented by an *n-1*-tuple of non-negative integers.
For example, the SU(2) singulet, duplet, and triplet are written as `Multiplet[{0}]`, `Multiplet[{1}]` and `Multiplet[{2}]`, respectively.
The product of two multiplets is represented by `MultipletProduct`, and irreducible representations can be found with `MultipletReduce`. 
For example

```mathematica
MultipletReduce@MultipletProduct[Multiplet[{1}], Multiplet[{1}]]
(* MultipletSum[Multiplet[{0}], Multiplet[{2}]] *)
``` 

shows that the product of two duplets is the sum of a singulet and a triplet (see, e.g., [https://en.wikipedia.org/wiki/Intersystem_crossing](https://en.wikipedia.org/wiki/Intersystem_crossing) for an application).
Here is a common example in SU(3), showing that a triplet and an anti-triplet form a singulet and an octet:

```mathematica
MultipletReduce@MultipletProduct[Multiplet[{0, 1}], Multiplet[{1, 0}]]
(* MultipletSum[Multiplet[{0, 0}], Multiplet[{1, 1}]] *)
``` 

By default, `Multiplet` expressions are displayed in the traditional "dimension" form, that is, when evaluated, `Multiplet[{1,0}]` is displayed as a blue "3", and `Multiplet[{0,1}]` is displayed as a blue "3" with a bar on top.
The form in which a multiplet is displayed can be specified with the options `StandardForm` and `TraditionalForm` of `Multiplet`.
For example, 

```mathematica
Multiplet[{0,1}, StandardForm -> "Plain"]
```

will prevent the traditional display in StandardForm.
Both options can be either `"Plain"`, `"Dimension"` (default), or `"Diagram"`, and you can also use `SetOptions[Multiplet, StandardForm -> "Diagram"]` to always display multiplets in diagramatic form.
The latter only works for SU(2) and SU(3) multiplets.
For example,

```mathematica
SetOptions[Multiplet, StandardForm -> "Diagram"];
MultipletProduct[Multiplet[{1, 2}], Multiplet[{0, 1}]]
```
![image missing](http://jem-mosig.com/wp-content/uploads/2018/02/dia_1-2_0-1a.png)

```mathematica
% // MultipletReduce
```
![image missing](http://jem-mosig.com/wp-content/uploads/2018/02/dia_1-2_0-1b.png)
