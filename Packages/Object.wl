(* ::Package:: *)

(* ::Section:: *)
(*PackageFramework*)


(* ::Text:: *)
(*A framework for extending the built-in BeginPackage/EndPackage-style package mechanism.*)
(*Provides developer utilities so that packages may be developed with less effort and more features.*)


PackageFrameworkPackage::usage=
  "PackageFrameworkPackage[ops] represents a package and provides methods on this package";
LoadPackage::usage=
	"LoadPackage[pkg] loads a PackageFrameworkPackage


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*PackageFrameworkPackage*)


(* ::Subsubsubsection::Closed:: *)
(*getPackageFrameworkConfig*)


getPackageFrameworkConfig::doc="
	Makes it possible to support the legacy design from when this was all part of BTools
and I expected a Config folder at the root of every package

The new design attempts to unify my two major use cases: 
	1) making new packages with my standard layout and which will auto-context for you
	2) hooking into existing packages for extension and introspection
";
getPackageFrameworkConfig[ops_, paclet_, rootDirectory_]:=
  Module[{pacletData = paclet, otherSettings = {}},
     Switch[pacletData,
       None,
         (* proceed to try to find the missing PacletInfo.m or PacletInfo.wl file *)
         pacletData = 
           Which[
             FileExistsQ[FileNameJoin@{rootDirectory, "PacletInfo.m"}],
               PacletManager`PacletInformation@
                 FileNameJoin@{rootDirectory, "PacletInfo.m"},
             FileExistsQ[FileNameJoin@{rootDirectory, "PacletInfo.wl"}],
               PacletManager`PacletInformation@
                 FileNameJoin@{rootDirectory, "PacletInfo.wl"},
             True,
               <||>
             ],
       _PacletManager`Paclet|_System`PacletObject,
         pacletData = PacletManager`PacletInformation@pacletData
       ];
    pacletData=
      Association@
        Replace[
          Lookup[pacletData, "Extensions", {}],
          {key_String, ops___}:>
            (key->Association@Flatten@{ops}),
          1
          ];
    Which[
     FileExistsQ[FileNameJoin@{rootDirectory, "Config", "LoadInfo.m"}],
       PacletManager`PacletInformation@
         FileNameJoin@{rootDirectory, "Config", "LoadInfo.m"},
     FileExistsQ[FileNameJoin@{rootDirectory, "Config", "LoadInfo.wl"}],
       PacletManager`PacletInformation@
         FileNameJoin@{rootDirectory, "Config", "LoadInfo.wl"}
     ];
    Join[
       <|
         "Name"->Lookup[pacletData, "Name"],
         "Location"->Lookup[pacletData, "Location"]
         |>,
       Replace[
         Lookup[Lookup[pacletData, "Kernel", <||>], "Context"],
         {
           m_Missing:>{FileBaseName@rootDirectory<>"`"},
           e_:>Flatten@{e}
           }
         ],
       Lookup[pacletData, "PackageFramework", <||>]
       ]//Merge[
       Flatten@{
         ops,
         #,
         Options[PackageFrameworkPackage]
         },
       First
       ]&
    ]


(* ::Subsubsubsection:: *)
(*PackageFrameworkPackage*)


PackageFrameworkPackage::doc="
Can be initialized directly from an association, from a paclet, or from a directory

Supports the following keys:
  Name: Package name
  Location: Package location
  Context: Root context(s) to use and from which to derive subcontexts
  PackageRoot: where to find packages (defaults to top-level Package directory)
  ResourceRoot: where to find resources (defaults to top-level Resource directory)
  LoadInfo: 
    Mode: the loading mechanism (Dependency or Primary) for the package
    FEHidden: the list of packages that should be hidden from the FE
    PackageScope: the list of packages that should be package scoped and not exposable
    PreLoad: the list of packages that should be loaded when the package is loaded
    Dependencies: the list of dependencies (with suboptions) for the package
  BundleInfo:
    RemovePaths: the set of paths to be removed from the bundled package
    RemovePatterns: the set of path patterns to be remove from the bundled package
";
Options[PackageFrameworkPackage]=
  {
    "Name"->Automatic,
    "Location"->Automatic,
    "Context"->Automatic,
    "PackageRoot"->"Packages",
    "ResourceRoot"->"Resources",
    "LoadInfo"->{
      "Mode"->"Primary",
      "FEHidden"->{},
      "PackageScope"->{},
      "PreLoad"->{},
      "Dependencies"->{}
      },
    "BundleInfo"->{
      "RemovePaths" -> {"Private", "project", "GitHub", ".git"}, 
      "RemovePatterns" -> {
         "Packages/*.nb", 
         "Packages/*/*.nb", 
         "Packages/*/*/*.nb", 
         ".DS_Store"
         }
      }
    };
PackageFrameworkPackage[
  pkg:_PacletManager`Paclet|_System`PacletObject,
  ops:OptionsPattern[]
  ]:=
  PackageFrameworkPackage@
    getPackageFrameworkConfig[{ops}, pkg, pkg["Location"]]
PackageFrameworkPackage[
  loc:_String?DirectoryQ,
  ops:OptionsPattern[]
  ]:=
  PackageFrameworkPackage@
    getPackageFrameworkConfig[{ops}, None, loc]
PackageFrameworkPackage[
  ops:OptionsPattern[]
  ]:=
  With[{loc=OptionValue["Location"]},
    PackageFrameworkPackage@
      getPackageFrameworkConfig[{ops}, None, loc]/;StringQ[loc]&&DirectoryQ[loc]
    ];
PackageFrameworkPackage[a_Association]?(
  Function[Null, Not@System`Private`ValidQ[Unevaluated[#]], HoldFirst]
  ):=
  With[{loc=a["Location"]},
    With[{config=getPackageFrameworkConfig[Normal[a], None, loc]},
      System`Private`SetValid@Unevaluated@PackageFrameworkPackage[config]
      ]/;StringQ[loc]&&DirectoryQ[loc]
    ]


(* ::Subsubsubsection:: *)
(*Accessors*)


PFPackageOption[PackageFrameworkPackage[a_Association], keys:__String]:=
  Fold[Lookup[#, #2, Return[Missing["KeyAbsent", #2], Fold]]&, a, {keys}];
PFPackageSetOption[PackageFrameworkPackage[a_Association], {keys__String}->value_]:=
  With[{aa=ReplacePart[a, {keys}->value]},
    System`Private`SetValid@Unevaluated@PackageFrameworkPackage[aa]
    ]


p_PackageFrameworkPackage?SystemPrivate`ValidQ[keys:__String]:=
  PFPackageOption[p, keys]
p_PackageFrameworkPackage?SystemPrivate`ValidQ[keys:__String]:=
  PFPackageOption[p, keys]


PackageFrameworkPackageMutate//ClearAll
PackageFrameworkPackageMutate~SetAttributes~HoldAllComplete
PackageFrameworkPackageMutate[
  s_Symbol?SystemPrivate`ValidQ[keys:__String]=value_
  ]:=
  (s=PFPackageSetOption[s, {keys}->value]);
PackageFrameworkPackageMutate[___]:=Language`MutationFallthrough


(* ::Subsubsection:: *)
(*LoadPackage*)


LoadPackage[
  pkg_PackageFrameworkPackage, 
  parent:_PackageFrameworkPackage|Automatic|None:None
  ]:=
  With[
    {
      $Name=PackageName[pkg], 
      $Head=PackageHead[pkg],
      $DependencyLoad=pkg["LoadInfo", "Mode"]==="Dependency"
      },
      Block[
        {
          $ParentPackage=
            Replace[parent, 
              Automatic:>If[$DependencyLoad, $Package, None]
              ],
          $Package=pkg,
          $ParentContext
          },
        $ParentContext = 
          Replace[{l_, ___}:>l]@
            Replace[$ParentPackage, 
              {
                None->"",
                e_:>e["Context"]
                }
              ];
        $Package["TopLevelLoad"]=
          If[$DependencyLoad,
            False,
            MemberQ[$ContextPath, "Global`"]
            ];
        Internal`WithLocalSettings[
          BeginPackage["`PackageFramework`"];
          ClearAll[$Head];,
        
        
          EndPackage[];
          ]
      ]
    ]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];
