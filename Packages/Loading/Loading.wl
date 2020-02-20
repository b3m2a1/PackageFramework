(* ::Package:: *)

(* ::Section:: *)
(*Loading*)


(* ::Text:: *)
(*Handles the heart of the loading process for the package by scanning the structure, setting up autoload rules, etc.*)


PackageExecute::usage="PackageExecute[pkg, expr] executes expr in the right context path";


PackagePullDeclarations::usage="PackagePullDeclarations[pkg] finds all the declared symbols \
in a package";
PackageAutoloadPackage::usage="PackageAutoloadPackage[pkg] loads a package via PackageExecute when the \
symbol it is bound to is used";
PackageLoadDeclare::usage="PackageLoadDeclare[pkg, file] handles the declaration process for single \
package file";


PackageFrameworkPackageLoad::usage="PackageFrameworkPackageLoad[pkg] handles the entire declaration \
process for a package";
PackageFrameworkPackageGet::usage=
  "Loads a specific subpackage file using Get with the appropriate directory structure";
PackageFrameworkPackageNeeds::usage=
  "Loads a specific subpackage file if it hasn't already been loaded";


PackageEnsureLoad::usage="PackageEnsureLoad[pkg] loads the package if it has not already been loaded";
PackageCompleteLoadProcess::usage="PackageCompleteLoadProcess[pkg] finalizes the load process for \
the package";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*PackageExecute*)


PackageExecute//Clear
PackageExecute[
  pkg_PackageFrameworkPackage, 
  expr_, 
  context:_String|Automatic:Automatic
  ]:=
  With[{ctxs=PackageContexts[pkg]},
    Block[{$CurrentPackage=pkg},
      Internal`WithLocalSettings[
        Begin[Replace[context, Automatic->ctxs[[1]]]];
        System`Private`NewContextPath@
          Prepend[ctxs, "System`"];,
        expr,
        System`Private`RestoreContextPath[];
        End[];
        ]
      ]
    ];
PackageExecute~SetAttributes~HoldRest


(* ::Subsubsection::Closed:: *)
(*PackagePullDeclarations*)


(* ::Text:: *)
(*This is another big place where we need to introduce greater flexibility*)
(*The current design looks for a few things:*)
(**)
(*Begin[...] / BeginPackage[...]*)
(**)
(*We need to add some flexbility to this design, though.*)
(**)
(*Any statement up until a Begin["`Private*`"] should be allowed to operate as normal (but the number of necessary End[] and EndPackage[] statements should be incremented). The reason for this is that we'd like to be able to handle any type of auto-completion as longs as it's generally within the traditional Mathematica package design framework.*)


(* ::Text:: *)
(*This basically means we need to handle a few distinct subcases of package layouts. We can have the classic:*)
(**)
(*  ExposedSymbolA::usage="....";*)
(*  ExposedSymbolB::usage="....";*)
(*  ExposedSymbolC::usage="....";*)
(**)
(*  Begin["`Private`"]*)
(*  (* End Here *)*)
(**)
(*But I'd also like to be able to appropriately handle the case of *)
(**)
(*  BeginPackage["MainPackageName`"]*)
(*  *)
(*  ExposedSymbolA::usage="....";*)
(*  ExposedSymbolB::usage="....";*)
(*  ExposedSymbolC::usage="....";*)
(**)
(*  Begin["`Private`"]*)
(*  (* End Here *)*)
(*  *)
(*On the other hand, we might also want to support something like:*)
(**)
(*  BeginPackage["`SubPackageName`"]*)
(*  *)
(*  ExposedSymbolA::usage="....";*)
(*  ExposedSymbolB::usage="....";*)
(*  ExposedSymbolC::usage="....";*)
(**)
(*  Begin["`Private`"]*)
(*  (* End Here *)*)
(**)
(*but in that case there's some question as to how we want to implement this. Should we be placing autocompletions onto these sub-symbols too?*)


(* ::Text:: *)
(*Another question that arises: Should we be overloading stuff like Begin and BeginPackage in this initialization phase? That would make it possible to not have to catch every possible pattern inside PackagePullDeclarationsAction, but at the same time runs the risk of massive breakage if things go wrong.*)
(**)
(*Something to think about in the future...*)


PackagePullDeclarationsAction//Clear
PackagePullDeclarationsAction[
  a:Hold[
    (h:Begin|BeginPackage)[e_, r___]|
    CompoundExpression[
      (h:Begin|BeginPackage)[e_, r___], 
      ___
      ]
    ]
  ]:=
  If[StringQ[e]&&StringStartsQ[e, "`Private"],
    Throw[End, $EndPackageDeclaration[$CurrentPackageFile]],
    h[e, r]; (* allow the Begin or BeginPackage to activate *)
    Switch[h, 
      Begin, $EndCallStack = {End, $EndCallStack},
      BeginPackage, $EndCallStack = {EndPackage, $EndCallStack}
      ];
    AssociateTo[$PackageContexts, $Context->None]
    ];
PackagePullDeclarationsAction[
  a:Hold[
    (h:End|EndPackage)[]|
    CompoundExpression[
      (h:End|EndPackage)[___], 
      ___
      ]
    ]
  ]:=
  (
    h[];
    If[Length@$EndCallStack == 2,
      $EndCallStack = $EndCallStack[[2]]
      ];
    );
PackagePullDeclarationsAction[e:Except[Hold[Expression]]]:=
  Sow@e;


PackagePullDeclarations[pkg_, pkgFile_]:=
  Module[{decStatements, stream, packageSymbols},
    Block[
      {
        $EndCallStack = {}, 
          (* 
            We'll sow in End and EndPackage statement as necessary so that we can just
            map over the flattened version of the stack after we're done with finding all
            of our package declarations
            *)
        $PackageLoadedContexts = 
          <| PackageFileContext[pkg, pkgFile] -> None |>,
        $CurrentPackageFile = pkgFile
        },
      Internal`WithLocalSettings[
        stream = OpenRead[pkgFile],
        decStatements = 
          Reap[
            Catch[
              Do[ 
                (* read the package block-by-block and cache the relevant symbols *)
                If[
                  Length[
                    ReadList[
                      stream,
                      PackagePullDeclarationsAction@Hold[Expression],
                      1
                      ]
                    ]===0,
                    Throw[
                      EndOfFile,
                      $EndPackageDeclaration[$CurrentPackageFile]
                      ]
                  ],
                Infinity
                ],
              $EndPackageDeclaration[$CurrentPackageFile]
             ]
          ][[2]],
        Close[stream];
        Map[#[]&, Flatten@$EndCallStack];
        ];
    packageSymbols=
      Cases[
        decStatements,
        s_Symbol?(
          Function[
            Null,
            Quiet[KeyExistsQ[$PackageLoadedContexts, Context[#]]],
            HoldAllComplete
            ]
            ):>
            HoldPattern[s],
        Infinity
        ];
    AddPackageSymbols[pkg, packageSymbols];
    PackageExtendContextPath[pkg, Keys@$PackageLoadedContexts];
    pkgFile->{Keys@$PackageLoadedContexts, packageSymbols}
    ] 
  ]


(* ::Subsubsection::Closed:: *)
(*PackageAutoloadPackage*)


(* ::Text:: *)
(*Handles the actual loading of a file. This is set up by the declarer to autoload all the package symbols.*)


PackageAutoloadPackage//Clear
PackageAutoloadPackage[pkg_PackageFrameworkPackage, heldSym_, context_, pkgFile_->syms_]:=
  Block[
    {
      $loadingChain=
        If[!AssociationQ@$inLoad, 
          $loadingChain = <| pkg -> {} |>,
          If[!KeyExistsQ[$loadingChain, pkg],
            Append[$loadingChain, pkg->{}],
            $loadingChain
            ]
          ],
      $inLoad=
        If[!AssociationQ@$inLoad, 
          <|pkg->False|>, 
          If[!KeyExistsQ[$inLoad, pkg],
            Append[$inLoad, pkg->False],
            TrueQ[$inLoad]
            ]
          ],
      $$inLoad=TrueQ[$inLoad[pkg]],
      ignored=With[{ss=pkg["AutoloadIgnored"]}, nonProtectedSym[ss]]
      },
    If[!MemberQ[$loadingChain[pkg], pkgFile],
      AppendTo[$loadingChain[pkg], pkgFile];
      $inLoad[pkg]=True;
      Internal`WithLocalSettings[
        (* holds the CPath so it can be restored later *)
        System`Private`NewContextPath@$ContextPath,
        
        (* Clear, but only for OwnValues *)
        Replace[syms,
          (s_Symbol?ignored)|Verbatim[HoldPattern][s_Symbol?ignored]:>
            (OwnValues[s]={}),
          1
          ];
        PackageEnsureLoad[pkg];
        PackageFrameworkPackageGet[pkg, context[[1]], pkgFile];
        PackageAddLoadedPackage[pkg, pkgFile],
          
        System`Private`RestoreContextPath[]
        ];
      ReleaseHold[heldSym]
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageDeclarePackage*)


(*CreatePackageSymbolInterface[$DeclaredPackages, "DeclaredPackages", PackageDeclaredPackages[#]&];*)


PackageDeclaredPackages[pkg_]:=
  Replace[
    PropertyValue[pkg, "DeclaredPackages"],
    _Missing:>(
      SetProperty[pkg, "DeclaredPackages"-><||>]; 
      <||>
      )
    ]


nonProtectedSym[protectedSyms_]:=
  Function[Null, nonProtectedSym[#, protectedSyms], HoldAllComplete];
nonProtectedSym[sym_, protectedSyms_]:=
  !MemberQ[protectedSyms, SymbolName@Unevaluated[sym]]
nonProtectedSym~SetAttributes~HoldAllComplete


PackageDeclarePackage[
  pkg_, 
  pkgFile_->{contexts_, syms_}
  ]:=
  With[{test=With[{e=pkg["AutoloadIgnored"]}, nonProtectedSym[e]]},
    SetProperty[pkg, {"DeclaredPackages", pkgFile}->syms];
    SetProperty[pkg, {"FileContexts", pkgFile}->contexts];
    Replace[
      syms,
      {
        (s:_Symbol?test)|
          (Verbatim[HoldPattern][s_Symbol?test]):>
          (
            If[MemberQ[Attributes[s], Protected],
              Unprotect[s]
              ];
            SetDelayed[
              s,
              PackageAutoloadPackage[pkg, HoldPattern[s], contexts, pkgFile->syms]
              ]
            ),
        e_->Nothing
        },
      1
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageLoadDeclare*)


PackageLoadDeclare//Clear
PackageLoadDeclare[pkg_PackageFrameworkPackage, pkgFile_String]:=
  If[!MemberQ[PackageLoadedPackages[pkg], pkgFile],
    If[!KeyMemberQ[PackageDeclaredPackages[pkg], pkgFile],
      If[AssociationQ[$PackageDeclarationList]&&KeyExistsQ[$PackageDeclarationList, pkg],
        $PackageDeclarationList[pkg]=
          Append[$PackageDeclarationList[pkg], PackagePullDeclarations[pkg, pkgFile]],
        PackageDeclarePackage[
          pkg,
          PackagePullDeclarations[pkg, pkgFile]
          ]
        ]
      ],
    PackageFrameworkPackageGet[pkg, pkgFile]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageLoad*)


BindPackageMethod["Load", PackageFrameworkPackageLoad];


PackageFrameworkPackageLoad//Clear
PackageFrameworkPackageLoad::doc="
Loops over all of the available files in the package and \
creates subcontexts for directories and configures auto-loading for the package files
";
PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage, dir_, listing_]:=
  Module[
    {
      fileNames=
        Select[
          FileNames["*", dir],
          DirectoryQ@#||MatchQ[FileExtension[#], "m"|"wl"]&
          ],
       preload,
       packages,
       postload
      },
    (* preloaded packages *)
    preload = 
      Select[fileNames, 
        StringMatchQ[
          ToLowerCase@FileNameTake[#],
          "__pre__."~~("m"|"wl")
          ]&
        ];
    If[Length@preload>0, Get[preload[[-1]]]];
    (* normally loaded packages *)
    packages = Select[fileNames, StringFreeQ["__"]@*FileBaseName];
    SetProperty[pkg, {"PackageListing", listing}->packages];
    PackageFrameworkPackageLoad[pkg, packages];
    (* packages loaded after all the others at that level *)
    postload = 
      Select[fileNames, 
        StringMatchQ[
          ToLowerCase@FileNameTake[#],
          "__post__."~~("m"|"wl")
          ]&
        ];
    If[Length@postload>0, Get[postload[[-1]]]];
    ];


PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage, dir_String?DirectoryQ]:=
  With[
    {baseContext = PackageFileContext[pkg, dir]},
    If[AllTrue[StringSplit[baseContext, "`"], StringMatchQ[(WordCharacter|"$")..]],
      Internal`WithLocalSettings[
        Begin[baseContext],
        PackageAddContexts[pkg, baseContext];
        PackageFrameworkPackageLoad[
          pkg,
          dir, 
          FileNameDrop[dir, FileNameDepth[PackageRoot[pkg, "Packages"]]]
          ],
        End[];
        ]
      ]
    ];
PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage, file_String?FileExistsQ]:=
  PackageLoadDeclare[pkg, file];
PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage]:=
  Block[
    {
      $PackageDeclarationList=
        If[!AssociationQ@$PackageDeclarationList, 
          <|pkg->{}|>, 
          Append[$PackageDeclarationList,pkg->{}]
          ]
      },
      PackageExecute[
        pkg,
        SetProperty[pkg, "PackageListing"-><||>];
        PackageFrameworkPackageLoad[
          pkg,
          PackageRoot[pkg, "Packages"],
          PackageName[pkg]
          ];
        PackageDeclarePackage[
          pkg,
          #
          ]&/@$PackageDeclarationList[pkg]
        ]
    ];
PackageFrameworkPackageLoad~SetAttributes~Listable;


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageGet*)


BindPackageMethod["Get", PackageFrameworkPackageGet]


PackageFrameworkPackageGet//Clear
PackageFrameworkPackageGet[pkg_PackageFrameworkPackage, f_]:=
  PackageFrameworkPackageGet[pkg, PackageFileContext[pkg, f], f];
PackageFrameworkPackageGet[pkg_PackageFrameworkPackage, c_, f_]:=
  PackageExecute[
      pkg,
      Get@PackageFileFromName[pkg, f],
      c
      ];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageNeeds*)


BindPackageMethod["Needs", PackageFrameworkPackageNeeds]


PackageFrameworkPackageNeeds[pkg_, pkgFile_String?FileExistsQ]:=
  If[!MemberQ[PackageLoadedPackages[pkg], pkgFile],
    If[KeyMemberQ[PackageDeclaredPackages[pkg], pkgFile],
      PackageLoadDeclare[pkg, pkgFile],
      Do[PackageLoadDeclare[pkg, pkgFile], 2] (* why twice...? *)
      ];
    ];


PackageFrameworkPackageNeeds[pkg_, pkgName_String]:=
  Module[{pf=PackageFilePath[pkg, "Packages", pkgName<>".m"]},
    If[!FileExistsQ@pf, PackageFilePath[pkg, "Packages", pkgName<>".wl"]];
    If[FileExistsQ@pf,
      PackageFrameworkPackageNeeds[pkg, pf],\.00 \.00
      $Failed
      ]
   ];


(* ::Subsubsection::Closed:: *)
(*PackageEnsureLoad*)


PackageEnsureLoad[pkg_]:=
  If[!TrueQ[PropertyValue[pkg, "LoadingFlag"]],
    SetProperty[pkg, "LoadingFlag"->True];
    PackageEnsureLoadDependencies[pkg];
    PackageExposeContexts[pkg];
    ];


(* ::Subsubsection::Closed:: *)
(*PackageCompleteLoadProcess*)


PackageCompleteLoadProcess::doc="
Handles stuff like making sure things that need to get pushed to a lower context do and that symbol \
coloring is turned off for the symbols where it should be turned off and all that
";
PackageCompleteLoadProcess[pkg_]:=
  (
    PackageFrameworkPackageGet[pkg, #]&/@
      PackagePreloadedPackages[pkg];
    PackageManageFrontEndSymbols[pkg];
    pkg
    )


(* ::Subsubsection::Closed:: *)
(*End*)


End[]
