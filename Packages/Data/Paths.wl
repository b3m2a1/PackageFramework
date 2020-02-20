(* ::Package:: *)

(* ::Section:: *)
(*Resource Finding*)


(* ::Text:: *)
(*Provides utilities for getting pieces out of a Package object*)


PackageRoot::usage="PackageRoot[pkg] returns the root directory for the package
PackageRoot[pkg, ctype] returns the root directory for the content type ctype";
PackageFileNames::usage="PackageFileNames[pkg, pat] returns the files in the package matching pat";
PackageFilePath::usage="PackageFilePath[pkg, path] returns the file path using PackageRoot as the root";
PackageFileFromName::usage="PackageFileFromName[pkg, name] returns the package file pointed to by name";
PackageFileContext::usage="PackageFileContext[pkg, file] gives the context associated with file";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*PackageRoot*)


BindPackageMethod["Directory", PackageRoot]


PackageRoot[PackageFrameworkPackage[a_]?PackageFrameworkPackageQ, Optional[None, None]]:=
  a["Location"];
PackageRoot[PackageFrameworkPackage[a_]?PackageFrameworkPackageQ, k_String]:=
  FileNameJoin@{
    a["Location"],
    Lookup[a, k<>"Root", k]
    }


(* ::Subsubsection::Closed:: *)
(*PackageFileNames*)


BindPackageMethod["FileNames", PackageFileNames]


PackageFileNames[
  pkg_PackageFrameworkPackage, 
  pattern_?StringPattern`StringPatternQ, 
  root:_String|None, 
  e___
  ]:=
  FileNames[pattern, PackageRoot[pkg, root]]


(* ::Subsubsection::Closed:: *)
(*PackageFilePath*)


BindPackageMethod["Path", PackageFilePath]


PackageFilePath[
  pkg_PackageFrameworkPackage,
  p_String,
  r___String
  ]:=
  FileNameJoin@{
    Replace[PackageRoot[pkg, p], _Missing->p],
    r
    };


(* ::Subsubsection::Closed:: *)
(*PackageFileFromName*)


PackageFileFromName//Clear
PackageFileFromName[pkg_PackageFrameworkPackage, f_]:=
  Module[
    {
      fBase=f
      },
    If[!FileExistsQ[fBase],
      fBase = PackageFilePath[pkg, "Packages", f<>".m"]
      ];
    If[!FileExistsQ[fBase],
      fBase = PackageFilePath[pkg, "Packages", f<>".wl"]
      ];
    fBase
    ];


(* ::Subsubsection::Closed:: *)
(*PackageFileContext*)


(* ::Text:: *)
(*Gotta make this more flexible.*)
(*The basic idea will be to allow the Package to declare its chosen "root" context, then we build off of that.*)
(*Subdirectory names _might_ be allowed to map themselves to a different context in the future?*)


PackageFileContextPath[pkg_PackageFrameworkPackage, f_String?DirectoryQ]:=
  FileNameSplit[
    FileNameDrop[
      f,
      FileNameDepth[PackageRoot[pkg, "Packages"]]
      ]
    ];
PackageFileContextPath[pkg_PackageFrameworkPackage, f_String?FileExistsQ]:=
  PackageFileContextPath[pkg, DirectoryName@f];


PackageFileContext[pkg_PackageFrameworkPackage, f_String?DirectoryQ]:=
  Module[
    {
      s=PackageFileContextPath[pkg, f], 
      root=StringTrim[PackageContext[pkg], "`"],
      contextRemapping = PackageContextMapping[pkg],
      cont
      },
    cont = StringRiffle[Append[""]@Prepend[s, root], "`"];
    Replace[cont, contextRemapping]
    ];
PackageFileContext[pkg_PackageFrameworkPackage, f_String?FileExistsQ]:=
  PackageFileContext[pkg, DirectoryName[f]];


(* ::Subsection:: *)
(*End*)


End[]
