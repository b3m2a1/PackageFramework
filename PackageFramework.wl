(* ::Package:: *)

(* ::Section:: *)
(*Package Loader*)


(* ::Text:: *)
(*Bootstrapping loading for the PackageFramework package*)


If[(* version of Needs that'll bootstrap *)
  !StringQ[PackageFramework`Package`$PackageFrameworkVersion]||
    StringStartsQ[
      PackageFramework`Package`$PackageFrameworkVersion, 
      "standalone"
      ],
  Block[{$Path=Prepend[$Path, DirectoryName@$InputFileName]},
    <<PackageFrameworkLoader`
    ];
  PackageFramework`LoadPackage[]
  ]
