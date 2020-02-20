(* ::Package:: *)

(* ::Section:: *)
(*LoadPackage*)


(* ::Text:: *)
(*This defines LoadPackage, the only function the framework actually exports at top level*)


LoadPackage::usage=
	"LoadPackage[pkg] loads a PackageFrameworkPackage";
PackageFrameworkPackage::usage=
  "PackageFrameworkPackage[ops] represents a package and provides methods\
 on this package";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*LoadPackage*)


(* ::Text:: *)
(*This is the heart of the system. Everything in this part of the package is basically in service to this method.*)


(* ::Text:: *)
(*It's not entirely clear at the moment how I want to make stuff like my autocompletions and front end coloring stuff work with the current layout.*)


(* ::Text:: *)
(*Realistically, there's no reason they even _need_ to be part of this system so I think I'll turn them into their own little paclets and go on my merry way.*)


(* ::Subsection:: *)
(*Load Package*)


(* ::Subsubsection:: *)
(*LoadPackage*)


(* ::Text:: *)
(*This is the heart of the system. Everything in this part of the package is basically in service to this method.*)


(* ::Text:: *)
(*Things like symbol coloring and autocompletions can come in the full version of the package. The redux version will leave them out.*)


ClearAll[LoadPackage]
LoadPackage::doc="

";
LoadPackage::npkg="`` is not a valid package";
Options[LoadPackage]:=
  {
    "Reload"->False,
    "PreloadHandler"->None,
    "PostloadHandler"->None,
    "Hook"->None
    };
LoadPackage[
  pkg_PackageFrameworkPackage, 
  parent:_PackageFrameworkPackage|Automatic|None:Automatic,
  ops:OptionsPattern[]
  ]:=
  With[
    {
      $Name=PackageName[pkg],
      pre=OptionValue["PreloadHandler"],
      post=OptionValue["PostloadHandler"],
      hook=OptionValue["Hook"],
      reload=TrueQ@OptionValue["Reload"]
      },
    If[reload,
      With[{objs=Lookup[$PackageFrameworkPackages, pkg@"Contexts"[]]},
        KeyDropFrom[$PackageFrameworkPackages, 
          Join[
            pkg@"Contexts"[],
            #@"Location"&/@objs
            ]
          ]
        ];
      RemoveProperty[pkg, All]
      ];
    Block[
      {
        $PackageLoadStack = 
          If[!ListQ@$PackageLoadStack, {}, {pkg, $PackageLoadStack}],
        $ParentPackage=
          Replace[
            parent,
            Automatic:>
              If[PackageLoadingMode[pkg]==="Dependency", 
                $CurrentPackage, 
                None
                ]
            ],
        $CurrentPackage=pkg,
        $ParentContext,
        $PackageContext,
        $PackageFrameworkSubcontext,
        $DependencyLoad=PackageLoadingMode[pkg]==="Dependency"
        },
      (* this has to be set first *)
      $ParentContext = 
        Replace[{l_, ___}:>l]@
          Replace[$ParentPackage, 
            {
              None->"",
              e_:>PackageContext[e]<>"Dependencies`"
              }
            ];
      $PackageContext =
        PackageContext[pkg, $ParentContext];
      $PackageFrameworkSubcontext =
        $PackageContext <> "PackageScope`";
      If[$ParentPackage =!= None && 
        !AssociationQ[$PackageFrameworkPackages[$ParentPackage]],
        $PackageFrameworkPackages[$ParentPackage] = <||>
        ];
      If[reload || 
          !KeyExistsQ[
            If[$ParentPackage =!= None,
              $PackageFrameworkPackages[$ParentPackage],
              $PackageFrameworkPackages
              ],
            $PackageContext
            ],
        If[$ParentPackage === None,
          $PackageFrameworkPackages[$PackageContext]=$CurrentPackage;
          $PackageFrameworkPackages[PackageRoot[$CurrentPackage]]=$CurrentPackage,
          $PackageFrameworkPackages[$ParentPackage, $PackageContext]=
            $CurrentPackage;
          $PackageFrameworkPackages[$ParentPackage, PackageRoot[$CurrentPackage]]=
            $CurrentPackage;
          ];
        SetProperty[$CurrentPackage, 
          "TopLevelLoad"->
            If[$DependencyLoad,
              False,
              MemberQ[$ContextPath, "Global`"]
              ]
          ];
        SetProperty[pkg, 
          (* should I use the current version of PackageContexts here...? *)
          "Contexts"->{$PackageContext, $PackageFrameworkSubcontext}
          ];
        SetProperty[pkg,
          "ParentContext"->$ParentContext
          ];
          
        Replace[hook, None->(#2&)][
          pkg,
          Internal`WithLocalSettings[
            BeginPackage[$PackageContext];
            PrependTo[$ContextPath, $PackageFrameworkSubcontext];
            pre[pkg],
            
            PackageConfigurePackageHelpers[pkg];
            PackageFrameworkPackageLoad[pkg];
            PackageCompleteLoadProcess[pkg];
            pkg,
            
            post[pkg];
            EndPackage[];
            ]
          ],
        pkg
        ]
      ]
    ];
LoadPackage[
  d_String?DirectoryQ, 
  parent:_PackageFrameworkPackage|Automatic|None:Automatic,
  ops:OptionsPattern[]
  ]:=
  With[{p=PackageFrameworkPackage[d]},
    LoadPackage[
      p,
      parent,
      ops
      ]/;PackageFrameworkPackageQ[p]
    ];
LoadPackage[
  ops:OptionsPattern[]
  ]:=
  With[{d=DirectoryName@$InputFileName},
    With[{p=PackageFrameworkPackage[d]},
      LoadPackage[
        p,
        ops
        ]/;PackageFrameworkPackageQ[p]
      ]/;StringLength[d]>0
    ];
LoadPackage[
  ctx_String?(StringEndsQ["`"]), 
  ops:OptionsPattern[]
  ]:=
  With[{i=FindFile[ctx]},
    LoadPackage[
      Nest[DirectoryName, i, 
        If[FileBaseName[i]==="init", 2, 1]
        ],
      ops
      ]/;StringQ[i]
    ];
LoadPackage[a_, ___]/;(Message[LoadPackage::npkg, a]):=Null;


(* ::Subsubsection:: *)
(*PackageFrameworkPackage*)


PackageFrameworkPackage::doc="
Can be initialized directly from an association, from a paclet, or from a directory

Supports the following keys:
  Name: Package name
  Location: Package location
  Context: Root context(s) to use and from which to derive subcontexts
  PackageRoot: where to find packages (defaults to top-level Package directory)
  ResourceRoot: where to find resources (defaults to top-level Resource directory)
  ExtraContexts: a set of extra contexts to expose once loading is done
  ContextMap: a mapping from standard context paths to custom ones
  Mode: the loading mechanism (Dependency or Primary) for the package
  DecoloredPackages: the list of packages that should be hidden from the FE
  PackageScope: the list of packages that should be package scoped and not exposable
  PreloadPackages: the list of packages that should be loaded when the package is loaded
  AutoloadIngnored: the list of symbol names to be ignored by the autoloader
  Dependencies: the list of dependencies (with suboptions) for the package
  PackageContexts: the list of contexts that should be available when loading
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
    "DependenciesRoot"->"Dependencies",
    "ExtraContexts"->{},
    "ContextMap"->{},
    "LoadingMode"->Automatic,
    "AutoloadIgnored"->{},
    "DecoloredPackages"->{},
    "PackageScope"->{},
    "PreloadPackages"->{},
    "Dependencies"->{},
    "RemovePaths" -> {"Private", "project", "GitHub", ".git"}, 
    "RemovePatterns" -> {
       "Packages/*.nb", 
       "Packages/*/*.nb", 
       "Packages/*/*/*.nb", 
       ".DS_Store"
       }
    };
PackageFrameworkPackage[
  pac:_PacletManager`Paclet|_System`PacletObject,
  parent:_PackageFrameworkPackage|None:None,
  ops:OptionsPattern[]
  ]:=
  With[{pkg=ConstructPackageFrameworkPackage[pac, parent, ops]},
    pkg/;pkg=!=$Failed
    ];
PackageFrameworkPackage[
  loc:_String?DirectoryQ,
  parent:_PackageFrameworkPackage|None:None,
  ops:OptionsPattern[]
  ]:=
  With[{pkg=ConstructPackageFrameworkPackage[loc, parent, ops]},
    pkg/;pkg=!=$Failed
    ];
PackageFrameworkPackage[
  ctx_String?(StringEndsQ["`"]), 
  parent:_PackageFrameworkPackage|None:None,
  ops:OptionsPattern[]
  ]:=
  With[{i=FindFile[ctx]},
    With[
      {
        pkg=ConstructPackageFrameworkPackage[
          Nest[
            DirectoryName, 
            i, 
            If[FileBaseName[i]==="init", 2, 1]
            ],
          parent,
          ops
          ]
          },
      pkg/;pkg=!=$Failed
      ]
    ];
PackageFrameworkPackage[
  ops:OptionsPattern[]
  ]:=
  With[{pkg=ConstructPackageFrameworkPackage[ops]},
    pkg/;pkg=!=$Failed
    ];
PackageFrameworkPackage[a_Association]?(
  Function[Null, Not@System`Private`ValidQ[Unevaluated[#]], HoldFirst]
  ):=
  With[{pkg=ConstructPackageFrameworkPackage@a},
    pkg/;pkg=!=$Failed
    ]


(* ::Subsection:: *)
(*End Private*)


End[];
