(* ::Package:: *)

(* ::Section:: *)
(*Resource Finding*)


(* ::Text:: *)
(*Provides utilities for getting pieces out of a Package object*)


PackageRoot::usage="";
PackageFileNames::usage="";
PackageFilePath::usage="";
PackageFEFile::usage="";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*PackageRoot*)


PackageRoot[PackageFrameworkPackage[a_], Optional[None, None]]:=
  a["Location"];
PackageRoot[PackageFrameworkPackage[a_], k_String]:=
  FileNameJoin@{
    a["Location"], 
    Lookup[a, k<>"Root", k]
    }


(* ::Subsubsection::Closed:: *)
(*PackageFileNames*)


PackageFileNames[
  pkg_PackageFrameworkPackage, 
  pattern_?StringPattern`StringPatternQ, 
  root:_String|None, 
  e___
  ]:=
  FileNames[pattern, PackageRoot[pkg, root]]


(* ::Subsubsection::Closed:: *)
(*PackageFilePath*)


PackageFilePath[
  pkg_PackageFrameworkPackage,
  p_String,
  r___String
  ]:=
  FileNameJoin@{
    PackageRoot[
      pkg,
      p
      ],
    r
    };
PackageFilePath[p__String]:=
  PackageFilePath[$CurrentPackage, p]


(* ::Subsubsection::Closed:: *)
(*PackageFEFile*)


PackageFEFile[pkg_PackageFrameworkPackage, p___, f_]:=
  FrontEnd`FileName[
    Evaluate@
      Flatten@{
        PackageName[pkg],
        p
        },
    f
    ]
PackageFEFile[p:Except[_PackageFrameworkPackage]..., f_]:=
  PackageFEFile[$CurrentPackage, p, f];


(* ::Subsubsection::Closed:: *)
(*PackagePathSymbol*)


PackagePathSymbol[parts___String,sym_String]:=
  ToExpression[StringRiffle[{$PackageName, parts, sym},"`"], StandardForm, HoldPattern];
PackagePathSymbol[parts___String,sym_Symbol]:=
  PackagePathSymbol[parts,Evaluate@SymbolName@Unevaluated[sym]];
PackagePathSymbol~SetAttributes~HoldRest;


(* ::Subsection:: *)
(*End*)


End[]
