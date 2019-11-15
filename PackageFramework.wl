(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Temp Loading Flag Code*)


Temp`PackageScope`PackageFrameworkLoading`Private`$PackageLoadData=
  If[#===None, <||>, Replace[Quiet@Get@#, Except[_?OptionQ]-><||>]]&@
    Append[
      FileNames[
        "LoadInfo."~~"m"|"wl",
        FileNameJoin@{DirectoryName@$InputFileName, "Config"}
        ],
      None
      ][[1]];
Temp`PackageScope`PackageFrameworkLoading`Private`$PackageLoadMode=
  Lookup[Temp`PackageScope`PackageFrameworkLoading`Private`$PackageLoadData, "Mode", "Primary"];
Temp`PackageScope`PackageFrameworkLoading`Private`$DependencyLoad=
  TrueQ[Temp`PackageScope`PackageFrameworkLoading`Private`$PackageLoadMode==="Dependency"];


(* ::Subsection:: *)
(*Main*)


If[Temp`PackageScope`PackageFrameworkLoading`Private`$DependencyLoad,
  If[!TrueQ[Evaluate[Symbol["`PackageFramework`PackageScope`Private`$LoadCompleted"]]],
    Get@FileNameJoin@{DirectoryName@$InputFileName, "PackageFrameworkLoader.wl"}
    ],
  If[!TrueQ[Evaluate[Symbol["PackageFramework`PackageScope`Private`$LoadCompleted"]]],
    <<PackageFramework`PackageFrameworkLoader`,
   BeginPackage["PackageFramework`"];
   EndPackage[];
   ]
  ]
