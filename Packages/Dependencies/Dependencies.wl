(* ::Package:: *)

(* ::Section:: *)
(*Dependencies*)


(* ::Text:: *)
(*Manages the loading of dependencies for a package*)


PackageLoadingMode::usage="PackageLoadingMode[pkg] returns whether the package is loaded as a \
\"Primary\" package or as a \"Dependency\"";
PackageDependencies::usage="PackageDependencies[pkg] returns the list of dependencies of the package";
PackageDependencyBase::usage="PackageDependencyBase[pkg] returns the server from which dependencies \
will be loaded";
PackageCheckPacletDependency::usage="PackageCheckPacletDependency[pkg, dep] checks that a paclet is \
installed";
PackageInstallPacletDependency::usage="PackageInstallPacletDependency[pkg, dep] installs a dependency \
paclet";
PackageUpdatePacletDependency::usage="PackageUpdatePacletDependency[pkg, dep] updates a dependency \
paclet";
PackageLoadPacletDependency::usage="PackageLoadPacletDependency[pkg, dep] loads a dependency paclet";
PackageEnsureLoadDependency::usage="PackageLoadPacletDependency[pkg, dep] loads a dependency paclet \
if is has not already been loaded";
PackageEnsureLoadDependencies::usage="PackageLoadPacletDependency[pkg] loads all of the dependencies \
of a package if they have not already been loaded";
PackageDependencyContexts::usage="PackageDependencyContexts[pkg] returns a context managed to be a \
dependency";
PackageExposeDependencies::usage="PackageExposeDependencies[pkg] adds the dependencies to $ContextPath";


PackageInstallPackageDependency::usage="";
PackageLoadPackageDependency::usage="";
PackageLoadResourceDependency::usage="";


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*PackageLoadingMode*)


PackageLoadingMode[pkg_]:=
  Replace[pkg["LoadingMode"],
    Automatic:>
      Replace[PropertyValue[pkg, "LoadingMode"],
        _Missing:>
          With[
            {
              m=
                If[TrueQ@$PackageFrameworkInDependencyLoad, "Dependency", "Primary"]
              },
            SetProperty[pkg, "LoadingMode"->m];
            m
            ]
        ]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageDependencies*)


PackageDependencies[pkg_PackageFrameworkPackage]:=
  pkg["Dependencies"]


(* ::Subsubsection::Closed:: *)
(*PackageDependencyBase*)


PackageDependencyBase[pkg_PackageFrameworkPackage]:=
  Replace[
    pkg["DependencyServer"],
    _Missing->Automatic
    ]


(* ::Subsubsection::Closed:: *)
(*PackageCheckPacletDependency*)


PackageCheckPacletDependency[dep_]:=
  (FindFile[dep]=!=$Failed)||
    (Length@PacletManager`PacletFind[StringDelete[dep, "`"]]>0)


(* ::Subsubsection::Closed:: *)
(*PackageInstallPacletDependency*)


PackageFrameworkPackage::nodep="Couldn't load dependency `` of type ``";


$PacletRepositoryServer="http://raw.githubusercontent.com/paclets/PacletServer/master";


Options[PackageInstallPacletDependency]=
  Options[PacletManager`PacletInstall];
PackageInstallPacletDependency[
  deps:{
    __String?(
      StringMatchQ[
        (LetterCharacter|"_"|"`")~~(WordCharacter|"_"|"`")..
        ]
    )}, 
  ops:OptionsPattern[]
  ]:=
  Block[{site, pacs, pac},
    pacs=
      StringDelete[deps, "`"];
    site=
      Replace[OptionValue["Site"],
        {
          s_String?(
            URLParse[#, "Domain"]==="github.com"&
            ):>
          URLBuild@
            <|
              "Scheme"->"http",
              "Domain"->"raw.githubusercontent.com",
              "Path"->
                Function[If[Length[#]==2, Append[#, "master"], #]]@
                  DeleteCases[""]@URLParse[s, "Path"]
              |>,
          None->
            Automatic,
          Except[_String]->
            $PacletRepositoryServer
          }
        ];
    pac=First@pacs;
    Monitor[
      MapThread[
        Check[
          PacletManager`PacletInstall[
            pac=#,
            "Site"->site,
            ops
            ],
          Message[PackageFrameworkPackage::nodep, #2, "Paclet"];
          $Failed
          ]&,
        {
          pacs,
          deps
          }
        ],
      Internal`LoadingPanel[
        TemplateApply[
          "Loading paclet `` from site ``",
          {pac, site}
          ]
        ]
      ]
    ]


PackageInstallPacletDependency[
  dep:_String?(
    StringMatchQ[
      (LetterCharacter|"_"|"`")~~(WordCharacter|"_"|"`")..
      ]
    ), 
  ops:OptionsPattern[]
  ]:=First@PackageInstallPacletDependency[{dep}, ops]


(* ::Subsubsection::Closed:: *)
(*PackageUpdatePacletDependency*)


PackageFrameworkPackage::nodup="Couldn't update dependency `` of type ``";


Options[PackageUpdatePacletDependency]=
  {
    "Sites"->Automatic
    };
PackageUpdatePacletDependency[
  deps:{__String?(StringMatchQ[(LetterCharacter|"_")~~(WordCharacter|"_")..])}, 
  ops:OptionsPattern[]
  ]:=
  Block[
    {
      added=<||>,
      ps=PacletManager`PacletSites[],
      pac
      },
    Replace[
      Replace[OptionValue["Sites"], Automatic:>{$PacletRepositoryServer}],
      {
        s_String:>
          If[!MemberQ[ps, PacletManager`PacletSite[s, ___]],
            added[s]=True
            ],
        p:PacletManager`PacletSite[__]:>
          If[!MemberQ[ps, p],
            added[p]=True
            ]
        },
      1
      ];
    pac=StringDelete[deps[[1]], "`"];
    Internal`WithLocalSettings[
      KeyMap[PacletManager`PacletSiteAdd, added],
      Monitor[
        MapThread[
          Check[
            PacletManager`PacletCheckUpdate[pac=#],
            Message[PackageFrameworkPackage::nodup, #2, "Paclet"];
            $Failed
            ]&,
          {
            StringDelete[deps, "`"],
            deps
            }
          ],
        Internal`LoadingPanel[
          "Updating paclet ``"~TemplateApply~pac
          ]
        ],
      KeyMap[PacletManager`PacletSiteRemove, added]
      ]
    ];


PackageUpdatePacletDependency[
  dep:_String?(StringMatchQ[(LetterCharacter|"_")~~(WordCharacter|"_")..]), 
  ops:OptionsPattern[]
  ]:=
  First@PackageUpdatePacletDependency[{dep}, ops]


(* ::Subsubsection::Closed:: *)
(*PackageLoadPacletDependency*)


Options[PackageLoadPacletDependency]=
  Join[
    Options[PackageInstallPacletDependency],
    {
      "Update"->False
      }
    ];
PackageLoadPacletDependency[pkg_, dep_String?(StringEndsQ["`"]), ops:OptionsPattern[]]:=
  Internal`WithLocalSettings[
    System`Private`NewContextPath[{dep, "System`"}];,
    If[PackageCheckPacletDependency[pkg, dep],
      If[TrueQ@OptionValue["Update"],
        PackageUpdatePacletDependency[
          pkg,
          dep,
          "Sites"->Replace[OptionValue["Site"], s_String:>{s}]
          ]
        ],
      PackageInstallPacletDependency[
        pkg,
        dep, 
        FilterRules[{ops}, Options@PackageInstallPacletDependency]
        ]
      ];
    Needs[dep];
    PackageExtendContextPath[
      pkg,
      Select[$Packages, 
        StringStartsQ[#, dep]&&StringFreeQ[#, "`*Private`"]&
        ]
      ];,
    System`Private`RestoreContextPath[];
    ]


(* ::Subsubsection::Closed:: *)
(*PackageEnsureLoadDependency*)


dependencyPackageMainPackageRoot[pkg_]:=
  Module[{pfp},
    pfp = 
      SplitBy[
        FileNameSplit[PackageRoot[pkg]],
        #==="Dependencies"&
        ];
    Reap[
      Fold[
        If[#2==="Dependencies", 
          Sow,
          Identity
          ]@
          FileNameJoin@Flatten@{#, #2}&,
        pfp
        ]
      ][[2]]
    ]


PackageEnsureLoadDependency//Clear
Options[PackageEnsureLoadDependency]=
  Join[
    Options@PackageLoadPacletDependency,
    {
      "Bundled"->True
      }
    ];
PackageEnsureLoadDependency[pkg_PackageFrameworkPackage, dep_, ops:OptionsPattern[]]:=
  Module[
    {
      depsDir,
      pfp,
      foundFile,
      bund=TrueQ@Quiet@OptionValue[PackageEnsureLoadDependency, ops, "Bundled"]
      },
     depsDir=
       If[PackageLoadingMode[pkg]==="Dependency",
         DeleteDuplicates@Flatten@{
          ParentDirectory@PackageRoot[pkg],
          PackageRoot[pkg, "Dependencies"],
          dependencyPackageMainPackageRoot[pkg]
         },
        {PackageRoot[pkg, "Dependencies"]}
        ];
     If[bund,
       If[AnyTrue[depsDir, DirectoryQ],
         foundFile=
           Block[
             {
               $Path=depsDir,
               PacletManager`PacletManagerEnabled
               },
             FindFile[dep]
             ];
         ];
       If[!StringQ@foundFile, 
         foundFile=FindFile[dep]
         ];
       bund=StringQ@foundFile
       ];
     Block[
       {
         $PackageFrameworkInDependencyLoad=True,
         $CurrentPackage=pkg
         },
       Quiet[(* this is a temporary hack until WRI fixes a $ContextPath bug *)
         If[!bund,
           PackageLoadPacletDependency[
             dep,
             Sequence@@
               FilterRules[
                 {
                   ops,
                   "Update"->True,
                   "Loading"->Get
                   },
                 Options@PackageLoadPacletDependency
                 ]
             ],
           Lookup[Flatten@{ops}, "Loading", Get]@
             foundFile
           (* I have my reasons to do this rather than Needs... but it could change... *)
           ],
        General::shdw
        ]
      ];
    ];


(* ::Subsubsection::Closed:: *)
(*PackageEnsureLoadDependencies*)


PackageEnsureLoadDependencies[pkg_PackageFrameworkPackage]:=
  If[!TrueQ@PropertyValue[pkg, "DependenciesLoadedFlag"],
    SetProperty[pkg, "DependenciesLoadedFlag"->True];
    Module[
      {
        deps=PackageDependencies[pkg],
        site=Replace[PackageDependencyBase[pkg], Automatic->$PacletRepositoryServer],
        ctx
        },
       PackageExecute[
         pkg,
         Internal`WithLocalSettings[
           ctx = $Context;
           Begin["`Dependencies`"],
           PackageEnsureLoadDependency[
             pkg,
             Sequence@@Flatten@{#, "Site"->site}
             ]&/@deps;
           PackageExposeDependencies[pkg],
           End[]; 
           (* just in case some abort happened in a dependency load *)
           $Context = ctx;
           ]
         ]
      ];
   ];


(* ::Subsubsection::Closed:: *)
(*PackageDependencyContexts*)


PackageDependencyContexts[pkg_]:=
  PackageDependencies[pkg][[All, 1]];


(* ::Subsubsection::Closed:: *)
(*PackageExposeDependencies*)


PackageExposeDependencies[pkg_, deps_, permanent:True|False:False]:=
  Module[
    {
      cdeps,
      loadQ,
      depCs
      },
    depCs=
      {
        PackageContexts[pkg][[1]]<>"Dependencies`",
        If[PackageLoadingMode[pkg]==="Dependency",
          StringSplit[PackageContexts[pkg][[1]], "Dependencies`", 2][[1]]<>
            "Dependencies`",
          Nothing
          ]
        };
    cdeps=
      With[{ctx=#},
        SelectFirst[
          depCs,
          Length@Join[Names[#<>ctx<>"*"], Names[#<>ctx<>"*`*"]]>0&,
          ""
          ]<>#
        ]&/@deps;
    (* only gonna be used within PackageExecute...? *)
    $ContextPath=
      DeleteDuplicates@
        Join[cdeps, $ContextPath];
    If[permanent,
      PackageExtendContextPath[pkg, cdeps]
      ];
    cdeps
    ];
PackageExposeDependencies[pkg_]:= 
  PackageExposeDependencies[
    pkg,
    PackageDependencyContexts[pkg],
    True
    ]


(* ::Subsubsection::Closed:: *)
(*PackageInstallPackageDependency*)


Options[PackageInstallPackageDependency]=
  {
    "Permanent"->False
    };
PackageInstallPackageDependency[dep_String, ops:OptionsPattern[]]:=
  Block[{retcode, site, path, file, tmp},
    path=
      StringSplit[StringTrim[dep, "`"]<>If[FileExtension[dep]=="", ".m", ""], "`"];
    site=
      Replace[OptionValue["Site"],
        {
          s_String?(
            URLParse[#, "Domain"]==="github.com"&
            ):>
          URLBuild@
            <|
              "Scheme"->"http",
              "Domain"->"raw.githubusercontent.com",
              "Path"->
                Function[If[Length[#]==2, Append[#, "master"], #]]@
                  DeleteCases[""]@URLParse[s, "Path"]
              |>,
          _->
            "http://raw.githubusercontent.com/paclets/PackageServer/master/Listing"
          }
        ];
      file=
        If[TrueQ@OptionValue["Permanent"],
          FileNameJoin@{$UserBaseDirectory, "Applications", Last@path},
          FileNameJoin@{$TemporaryDirectory, "Applications", Last@path}
          ];
      tmp=CreateFile[];
      Monitor[
        retcode=URLDownload[URLBuild[Prepend[site], path], tmp, "StatusCode"],
        Internal`LoadingPanel[
          TemplateApply[
            "Loading package `` from site ``",
            {URLBuild@path, site}
            ]
          ]
        ];
      If[retcode<300,
        CopyFile[tmp, file,
          OverwriteTarget->Not@TrueQ@OptionValue["Permanent"]
          ];
        DeleteFile[tmp];
        file,
        Message[$Name::nodep, dep, "Package"];
        DeleteFile[tmp];
        $Failed
        ]
      ];


(* ::Subsubsection::Closed:: *)
(*PackageLoadPackageDependency*)


Options[PackageLoadPackageDependency]=
  Append[
    Options[PackageInstallPackageDependency],
    "Loading"->Needs
    ];
PackageLoadPackageDependency[dep_String, ops:OptionsPattern[]]:=
  Internal`WithLocalSettings[
    BeginPackage[dep];,
    If[Quiet@Check[OptionValue["Loading"][dep], $Failed]===$Failed&&
        Quiet@Check[
          Get[FileNameJoin@@
            StringSplit[
              StringTrim[dep, "`"]<>If[FileExtension[dep]=="", ".m", ""], 
              "`"
              ]
            ], 
          $Failed]===$Failed,
      Replace[PackageInstallPacletDependency[dep, ops],
        f:_String|_File:>Get[f]
        ]
      ];
    PackageExtendContextPath@
      Select[$Packages, StringStartsQ[dep]];,
    EndPackage[];
    ]


(* ::Subsubsection::Closed:: *)
(*PackageLoadResourceDependency*)


(* ::Text:: *)
(*Nothing I've implemented yet, but could be very useful for installing resources for a paclet*)


(* ::Subsection::Closed:: *)
(*End*)


End[]
