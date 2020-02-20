(* ::Package:: *)

(* ::Section:: *)
(*Package Data*)


(* ::Text:: *)
(*Originally these were the constants that every package set in its load script*)
(*Now that there's a unified loader architecture, these are all functions that take a PackageFrameworkPackage object and return data from that object*)


PackageName::usage="PackageName[pkg] pulls the name out of pkg";
PackageHead::usage="PackageHead[pkg] returns the Head for pkg";
PackageContext::usage="PackageContext[pkg] returns the main package context";
PackageContexts::usage="PackageContexts[pkg] returns the set of Contexts used by\
 the package";
PackageExtendContextPath::usage="";
PackageSymbols::usage="PackageSymbols[pkg] returns the symbols provided by the package";
AddPackageSymbols::usage="AddPackageSymbols[pkg, syms] adds the symbols to the\
 package symbol list";
PackageFileContexts::usage="PackageFileContexts[pkg] The contexts for the files in pkg";
PackageDeclaredPackages::usage="PackageDeclaredPackages[pkg] the set of packages\
 found and declared via the autoloader for pkg";
PackageLoadedPackages::usage="PackageLoadedPackages[pkg] the set of already loaded\
 packages";
PackageAddLoadedPackages::usage="PackageAddLoadedPackages[pkg, files] adds the\
 files to the loaded package list";
PackagePreloadedPackages::usage="PackagePreloadedPackages[pkg] gives the set of\
 package to load immediately"; 


AllowPackageRecoloring::usage="AllowPackageRecoloring[pkg] returns whether or not\
 the package supports recoloring of symbols";
PackageUncoloredSymbols::usage="PackageUncoloredSymbols[pkg] returns the set of\
 symbols that shouldn't be colored by the FE";


AllowPackageRescoping::usage="AllowPackageRecoloring[pkg] returns whether or not the\
 package supports rescoping of symbols";
PackageScopedSymbols::usage="PackageScopedSymbols[pkg] returns the set of symbols that\
 should be rescoped to a package context";
AllowPackageSymbolDefinitions::usage="AllowPackageSymbolDefinitions[pkg] returns\
 whether or not the package supports a manager symbol pattern";
AllowPackageAutocompletions::usage="AllowPackageAutocompletions[pkg] returns\
 whether or not the package supports rescoping of symbols";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*PackageName*)


BindPackageMethod["Name", PackageName]


PackageName//Clear
PackageName[pkg_PackageFrameworkPackage]:=
  pkg["Name"];


(* ::Subsubsection::Closed:: *)
(*PackageHead*)


BindPackageMethod["Head", PackageHead]


PackageHead[pkg_]:=
  With[{c=PackageContext[pkg], n=PackageName[pkg]},
    Symbol[c<>StringDelete[n, Except["$"|WordCharacter]]]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageRootContext*)


PackageRootContext//Clear
PackageRootContext[pkg_PackageFrameworkPackage]:=
  Replace[PackageFrameworkPackageOptionValue[pkg, "Context"], {p_, ___}:>p]


(* ::Subsubsection::Closed:: *)
(*PackageContext*)


BindPackageMethod["Context", PackageContext]


PackageContext//Clear
PackageContext[pkg_PackageFrameworkPackage, root_String]:=
  root<>PackageRootContext[pkg];
PackageContext[pkg_PackageFrameworkPackage, Optional[Automatic, Automatic]]:=
  PackageContext[pkg, 
    Replace[PropertyValue[pkg, "ParentContext"], Except[_String]->""]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageContexts*)


BindPackageMethod["Contexts", PackageContexts]


(*CreatePackageSymbolInterface[$PackageContexts, "Contexts", PackageContexts[#]&];*)


PackageContexts[pkg_]:=
  Replace[
    PropertyValue[pkg, "Contexts"],
    _Missing:>(SetProperty[pkg, "Contexts"->{}])
    ]


(* ::Subsubsection::Closed:: *)
(*PackageExtendContextPath*)


BindPackageMethod["AddContexts", PackageExtendContextPath]


PackageExtendContextPath[pkg_, cp:{__String}]:=
  SetProperty[
    pkg,
    "Contexts"->
      DeleteCases[
        DeleteDuplicates@
          Join[PackageContexts[pkg], cp],
        "System`"|"Global`"
        ]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageExposeContexts*)


PackageExposeContexts[pkg_]:=
  If[ListQ@pkg["ExtraContexts"],
    PackageExtendContextPath[
      pkg,
      pkg["ExtraContexts"]
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageContextMapping*)


PackageContextMapping[pkg_PackageFrameworkPackage]:=
  pkg["ContextMap"]


(* ::Subsubsection::Closed:: *)
(*PackageInitialContextPath*)


PackageInitialContextPath::doc="
PackageInitialContextPath[pkg]

Converts the package layout into a set of contexts that will be global to the package
Checks first against the contexts that the package asks to expose

We assumed originally that $Context would be the lookup root for the 
main package itself, but this assumption can be relaxed at this point
As long as PackageContext[pkg] provides the fully-qualified name of the package 
we should be able to very normally manage the dependency packages

The second core assumption was that the directory structure would directly
map onto the $PackageContextPath, but this might not be how we want to do it

The only workable alternative to this is to force each directory to map onto a
context but provide a mapping that would allow us to revise the used context
at resolution time

e.g. We'd have Pkg/Sub/Subsub \[Rule] PkgContext`Sub`Subsub` but then we 
have a post-processing map that takes PkgContext`Sub`Subsub` \[Rule] PkgContext`NewPath`
";
PackageInitialContextPath[pkg_, includeOrig:True|False:True]:=
  Module[
    {
      packages,
      packageRoot=PackageRoot[pkg],
      packagePackageRoot = PackageRoot[pkg, "Packages"], 
      (* I regret this name already... *)
      name = PackageName[pkg],
      cont = PackageRootContext[pkg],
      opath = If[includeOrig, $ContextPath, {"System`"}],
      contextRemapping = PackageContextMapping[pkg],
      (* we'll add some kind of option to allow this to be remapped *)
      rootDepth,
      packageDirectories,
      packageContextPath
      },
    rootDepth = FileNameDepth@packagePackageRoot;
    packageDirectories = FileNames["*", packagePackageRoot, Infinity];
    packageDirectories = Select[packageDirectories, DirectoryQ];
    packageDirectories = 
      StringReplace[
        FileNameDrop[#, rootDepth],
        $PathnameSeparator->"`"
        ]&/@packageDirectories;
    packageDirectories = 
      Select[
        packageDirectories,
        StringMatchQ[("$"|WordCharacter)..]
        ];
    packageContextPath = 
      Join[
        opath,
        cont<>#&/@packageDirectories
        ];
    DeleteDuplicates[
      packageContextPath /. contextRemapping
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageFileContexts*)


(*CreatePackageSymbolInterface[$PackageFileContexts, "FileContexts", PackageFileContexts[#]&];*)


PackageFileContexts[pkg_]:=
  Replace[
    PropertyValue[pkg, "FileContexts"],
    _Missing:>(
      SetProperty[pkg, "FileContexts"-><||>];
      <||>
      )
    ]


(* ::Subsubsection::Closed:: *)
(*PackageLoadedPackages*)


(*CreatePackageSymbolInterface[$LoadedPackages, "LoadedPackages", PackageLoadedPackages[#]&];*)


PackageLoadedPackages[pkg_]:=
  Replace[
    PropertyValue[pkg, "LoadedPackages"],
    _Missing:>(
      SetProperty[pkg, "LoadedPackages"->{}];
      {}
      )
    ]


(* ::Subsubsection::Closed:: *)
(*PackageAddLoadedPackages*)


PackageAddLoadedPackages[pkg_, pkgFiles_]:=
  SetProperty[pkg, 
    "LoadedPackages"->
      DeleteDuplicates@Flatten[{PropertyValue[pkg, "LoadedPackages"], pkgFiles}]
    ]


(* ::Subsubsection::Closed:: *)
(*PackagePreloadedPackages*)


PackagePreloadedPackages[pkg_]:=
  pkg["PreloadPackages"]


(* ::Subsubsection::Closed:: *)
(*PackageSymbols*)


(*CreatePackageSymbolInterface[$PackageSymbols, "Symbols", PackageSymbols[#]&];*)


BindPackageMethod["Symbols", PackageSymbols]


PackageSymbols[pkg_]:=
  Replace[
    PropertyValue[pkg, "Symbols"],
    _Missing:>(
      SetProperty[pkg, "Symbols"->{}];
      {}
      )
    ]


(* ::Subsubsection::Closed:: *)
(*AddPackageSymbols*)


AddPackageSymbols[pkg_, pkgFiles_]:=
  SetProperty[pkg, 
    "Symbols"->
      DeleteDuplicates@Flatten[{PackageSymbols[pkg], pkgFiles}]
    ]


(* ::Subsubsection::Closed:: *)
(*Coloring*)


AllowPackageRecoloring[pkg_]:=
  Replace[
    pkg["AllowRecoloring"],
    Except[True|False]->(PackageLoadingMode[pkg]=!="Dependency")
    ];
PackageUncoloredSymbols[pkg_]:=
  Replace[
    PropertyValue[pkg, "FEHiddenSymbols"],
    _Missing:>(SetProperty[pkg, "FEHiddenSymbols"->{}];{})
    ]
AddUncoloredSymbols[pkg_, syms_]:=
  SetProperty[pkg, "FEHiddenSymbols"->Flatten@{PackageUncoloredSymbols[pkg], syms}];


(* ::Subsubsection::Closed:: *)
(*Rescoping*)


AllowPackageRescoping[pkg_]:=
  Replace[
    pkg["AllowRescoping"],
    Except[True|False]->(PackageLoadingMode[pkg]=!="Dependency")
    ];
PackageRescopedSymbols[pkg_]:=
  Replace[
    PropertyValue[pkg, "RescopedSymbols"],
    _Missing:>(SetProperty[pkg, "RescopedSymbols"->{}];{})
    ]
AddRescopedSymbols[pkg_, syms_]:=
  SetProperty[pkg, "RescopedSymbols"->Flatten@{PackageRescopedSymbols[pkg], syms}];


(* ::Subsubsection::Closed:: *)
(*AllowPackageSymbolDefinitions*)


AllowPackageSymbolDefinitions[pkg_]:=
  Replace[
    pkg["AllowSymbolDefinitions"],
    Except[True|False|None]->True
    ];


(* ::Subsubsection::Closed:: *)
(*AllowPackageAutocompletions*)


AllowPackageAutocompletions[pkg_]:=
  Replace[
    pkg["AllowSymbolDefinitions"],
    Except[True|False]->(PackageLoadingMode[pkg]=!="Dependency")
    ];


(* ::Subsubsection::Closed:: *)
(*End*)


End[]
