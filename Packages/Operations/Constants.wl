(* ::Package:: *)

(* ::Section:: *)
(*Constants*)


(* ::Text:: *)
(*Originally these were the constants that every package set in its load script*)
(*Now that we're moving to a single loader _package_ these will all be references to a $CurrentPackage object*)
(*that will be bound by PackageFramework`LoadPackage[...], which will be the main entry point into loading*)
(*packages these days*)


(* ::Subsection:: *)
(*Constants*)


$CurrentPackage::usage="The current package as configured by the loader";


$PackageDirectory::usage="";
$PackageName::usage="";
$PackageListing::usage="The listing of packages";
$PackagePackagesDirectory::usage="The directory to look for packages under";
$PackageContexts::usage="The list of contexts exposed to all packages";
$PackageDeclared::usage="Whether the package has been auto-loaded or not";
$PackageFEHiddenSymbols::usage="";
$PackageScopedSymbols::usage="";
$PackageLoadSpecs::usage="";
$PackageLoadingMode::usage=
  "A flag that determines whether the package is primary or a dependency";
$PackageDependencies::usage=
   "A set of dependencies for the package to load";
$PackageDependencyBase::usage=
  "The server from which dependencies should be loaded";
$AllowPackageSymbolDefinitions::usage="";
$AllowPackageRescoping::usage="";
$AllowPackageRecoloring::usage="";
$AllowPackageAutocompletions::usage="";


(* ::Subsubsection:: *)
(*Begin*)


Begin["`Constants`"];


(* ::Subsubsection:: *)
(*Naming*)


$Name["Directory"]:=
  $PackageDirectory;
$PackageDirectory=
  DirectoryName@$InputFileName;


$Name["Name"]:=
  $PackageName;
$PackageName=
  "$Name";


(* ::Subsubsection:: *)
(*Load Specs*)


$Name["LoadingParameters"]:=$PackageLoadSpecs
$PackageLoadSpecs=
  Merge[
    {
      With[
        {
          f=
            Append[
              FileNames[
                "LoadInfo."~~"m"|"wl",
                FileNameJoin@{$PackageDirectory, "Config"}
                ],
              None
              ][[1]]
          },
        Replace[
            Quiet[
              Import@f,
              {
                Import::nffil,
                Import::chtype
                }
              ],
          Except[KeyValuePattern[{}]]:>
            {}
          ]
        ],
      With[
        {
          f=
            Append[
              FileNames[
                "LoadInfo."~~"m"|"wl",
                FileNameJoin@{$PackageDirectory, "Private", "Config"}
                ],
              None
              ][[1]]},
        Replace[
          Quiet[
            Import@f,
            {
              Import::nffil,
              Import::chtype
              }
            ],
          Except[KeyValuePattern[{}]]:>
            {}
          ]
        ]
      },
    Last
    ];


(* ::Subsubsection:: *)
(*Mode*)


$Name["Mode"]:=$PackageLoadingMode
$PackageLoadingMode=
  Lookup[$PackageLoadSpecs, "Mode", "Primary"];


(* ::Subsubsection:: *)
(*Dependencies*)


$Name["Dependencies"]:=$PackageDependencies;
$PackageDependencies=
  Lookup[$PackageLoadSpecs, "Dependencies", {}];


(* ::Subsubsection:: *)
(*DependencyBase*)


$Name["DependencyBase"]:=$PackageDependencyBase;
$PackageDependencyBase=
  Lookup[$PackageLoadSpecs, "DependencyBase", {}];


(* ::Subsubsection:: *)
(*Loading*)


$Name["PackageListing"]:=$PackageListing;
$PackageListing=<||>;
$Name["Contexts"]:=$PackageContexts;
If[!ListQ@$PackageContexts,
  $PackageContexts=
    Join[
      If[$PackageLoadingMode==="Dependency",
        $RootContext<>#&/@#,
        #
        ]&@
        {
          "$Name`",
          "$Name`PackageScope`Private`",
          "$Name`PackageScope`Package`"
          },
      Lookup[$PackageLoadSpecs, "ExtraContexts", {}]
      ]
  ];
$PackageDeclared=
  TrueQ[$PackageDeclared];


$PackagePackagesDirectory=
  Replace[
    Lookup[$PackageLoadSpecs, "PackagesDirectory"],
    Except[s_String?(Directory@FileNameJoin@{$PackageDirectory, #}&)]->"Packages"
    ]


(* ::Subsubsection:: *)
(*Scoping*)


$Name["FEScopedSymbols"]:=$PackageFEHiddenSymbols;
$PackageFEHiddenSymbols={};
$Name["PackageScopedSymbols"]:=$PackageScopedSymbols;
$PackageScopedSymbols={};


(* ::Subsubsection:: *)
(*Allow flags*)


$AllowPackageSymbolDefinitions=
  Replace[
    Lookup[$PackageLoadSpecs, "PackageSymbolDefinitions"],
    Except[True|False|None]->True
    ];
$Name["AllowRescoping"]:=$AllowPackageRescoping;
$AllowPackageRescoping=
  Replace[
    Lookup[$PackageLoadSpecs, "AllowRescoping"],
    Except[True|False]->($PackageLoadingMode==="Primary")(*$TopLevelLoad*)
    ];
$Name["AllowRecoloring"]:=$AllowPackageRecoloring;
$AllowPackageRecoloring=
  Replace[
    Lookup[$PackageLoadSpecs, "AllowRecoloring"],
    Except[True|False]->($PackageLoadingMode==="Primary")(*$TopLevelLoad*)
    ];
$Name["AllowAutocompletions"]:=$AllowPackageAutocompletions;
$AllowPackageAutocompletions=
  Replace[
    Lookup[$PackageLoadSpecs, "AllowAutocompletions"],
    Except[True|False]->($PackageLoadingMode==="Primary")(*$TopLevelLoad*)
    ];


(* ::Subsubsection:: *)
(*End*)


End[]
