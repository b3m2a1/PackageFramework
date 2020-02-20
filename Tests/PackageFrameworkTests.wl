(* ::Package:: *)

(* ::Title:: *)
(*Package Framework Tests*)


(* ::Text:: *)
(*Tests how wells the package framework works*)


(* ::Subsection:: *)
(*PackageFrameworkTests*)


(* ::Subsubsection:: *)
(*BeginTestSuite*)


BeginTestSuite["PackageFrameworkTests", 
  Automatic, 
  {
    "PackageFramework`",
    "PackageFramework`Data`",
    "PackageFramework`Dependencies`",
    "PackageFramework`Loading`",
    "PackageFramework`Package`"
    }
  ]


(*SetTestSuiteOptions["Setup"\[Rule]Echo]*)


(* ::Subsubsection::Closed:: *)
(*LoadPackage*)


testLoadPackage[ctx_]:=
  Module[
    {pkg},
    pkg = LoadPackage[ctx];
    AssertTest[
      pkg, 
      PackageFrameworkPackageQ, 
      "Tag"->"ValidPackage"
      ]
    ];


TestCase["LoadPackage", testLoadPackage, "PackageFramework`"]


(* ::Subsubsection::Closed:: *)
(*PackageContexts*)


testPackageContexts[ctx_]:=
  Module[
    {pkg, ctxs},
    pkg = LoadPackage[ctx];
    ctxs = pkg@"Contexts"[];
    AssertTest[ctxs, ListQ, "Tag"->"Contexts"];
    ctxs
    ];


TestCase["PackageContexts", testPackageContexts, "PackageFramework`"]


(* ::Subsubsection:: *)
(*Deps*)


testPackageDeps[ctx_]:=
  Module[
    {pkg, ctxs},
    pkg = LoadPackage[ctx];
    ctxs = pkg@"Dependencies";
    AssertTest[ctxs, ListQ, "Tag"->"Dependencies"];
    ctxs
    ];


TestCase["PackageDependencies", testPackageDeps, "PacletBuilder`"]


(* ::Subsubsection:: *)
(*End*)


EndTestSuite[]


(* ::Section:: *)
(*Run Tests*)


(* ::Input:: *)
(*RunTests["PackageFramework"]*)
