(* ::Package:: *)

(* ::Section:: *)
(* Package Functions *)


(* ::Subsection:: *)
(*Post-Processing*)


PackagePrepPackageSymbol::usage="PackagePrepPackageSymbol[pkg] configures the PackageHead if that \
has been enabled for the package";


PackageExposeFrontEndSymbols::usage="PackageExposeFrontEndSymbols[pkg] makes the \
symbols that were hidden from the front end visible";
PackageHideFrontEndSymbols::usage="PackageHideFrontEndSymbols[pkg] hides the symbols that need to be \
hidden from the front end";
PackageDecontextPackages::usage="PackageDecontextPackages[pkg] moves symbols to a hidden context if \
they need to be hidden";


PackageContextPathReassign::usage="PackageContextPathReassign[pkg] resets the $ContextPath \
so that only the things that should be visible have been made visible";


PackageAttachMainAutocomplete::usage="PackageAttachMainAutocomplete[pkg] attaches autocomplete rules \
to the package head if they should be added";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*PackageNamePattern*)


getPackageNamePattern[fn_, root_]:=
    Replace[
      FileNameSplit@
        If[FileExistsQ@fn,
          FileNameDrop[ExpandFileName@fn, FileNameDepth@root],
          fn
          ],
      {
        {f_}:>
          f|fn|StringTrim[f, ".m"|".wl"],
        {p__,f_}:>
          FileNameJoin@{p,f}|fn|{p, StringTrim[f,".m"|".wl"]}
        }
      ]


(* ::Subsubsection:: *)
(*ExposePackages*)


(*PackageManageHiddenPackage::docs="
Takes any package that's not declared as hidden
"
PackageManageHiddenPackages[pkg_]:=
  Module[
    {
      hiddenPackages= PackageHiddenPackages[pkg],
      pkgRoot = PackageRoot[pkg, "Packages"],
      decs = PackageDeclaredPackages[pkg],
      hiddenSyms,
      exposedSyms
      },
    exposedSyms=
      If[!MemberQ[hiddenPackages, getPackageNamePattern[#, pkgRoot]],
        decs[#],
        {}
        ]&/@Keys@decs//Flatten;
    hiddenSyms=
      If[!MemberQ[hiddenPackages, getPackageNamePattern[#, pkgRoot]],
        decs[#],
        {}
        ]&/@Keys@decs//Flatten;
    Replace[
      Thread[
        If[ListQ@$PackageFEHiddenSymbols,
          DeleteCases[
            exposedSyms,
            Alternatives@@
              (Verbatim[HoldPattern]/@Flatten@$PackageFEHiddenSymbols)
            ],
          exposedSyms
          ],
        HoldPattern],
      Verbatim[HoldPattern][{s__}]:>
        AddFrontEndSymbolColoring[s]
      ]
    ]*)


(* ::Subsubsection:: *)
(*Decontext*)


(*PackagePostProcessDecontextPackages[]/;TrueQ[$AllowPackageRecoloring]:=
  (
    If[
      MemberQ[$PackageDecontextedPackages,
        PackagePostProcessFileNamePrep[#]
        ],
      PackageFERehidePackage@#;
      PackageDecontext@#
      ]&/@Keys@$DeclaredPackages;
    If[ListQ@$PackageScopedSymbols,
      KeyValueMap[
        With[{newcont=#},
          Replace[Join@@#2,
            HoldComplete[s__]:>
              (
                PackageFERehideSymbols[s];
                Map[
                  Function[Null,
                    Quiet[
                      Check[
                        Set[Context[#],newcont],
                        Remove[#],
                        Context::cxdup
                        ],
                      Context::cxdup
                      ],
                    HoldAllComplete
                    ],
                  HoldComplete[s]
                  ]//ReleaseHold;
                )
            ]
          ]&,
        GroupBy[Flatten@$PackageScopedSymbols,First->Last]
        ];
      ]
    )

*)


(* ::Subsubsection:: *)
(*ContextPathReassign*)


(*PackagePostProcessContextPathReassign[pkg_]:=
  With[
    {
      cp=$ContextPath, 
      mctx=PackageContext[pkg]
      },
    If[MemberQ[cp],
      mctx,
      $ContextPath=
        Join[
          Replace[
            Flatten@{PackageExposedContexts[pkg]},
            Except[_String?(StringEndsQ["`"])]->Nothing,
            1
            ],
          $ContextPath
          ];
      If[TrueQ[AllowPackageSymbolDefinitions[pkg]]&&$Notebooks, 
        FrontEnd`Private`GetUpdatedSymbolContexts[]
        ];
      ]
    ]*)


(* ::Subsubsection:: *)
(*AttachMainAutocomplete*)


(*PackageAttachMainAutocomplete[pkg_]:=
  With[{$Name=PackageHead[pkg]},
    PackageAddAutocompletions[
      $Name, 
      Table[
        Replace[{}->None]@
          Cases[
            DownValues[$Name],
            Nest[
              Insert[#, _, {1, 1, 1}]&,
              (HoldPattern[Verbatim[HoldPattern]][
                $Name[s_String, ___]
                ]:>_),
              n-1
              ]:>s,
            Infinity
            ],
        {n, 5}
        ]
      ]
    ];*)


(* ::Subsubsection:: *)
(*PackagePrepPackageSymbol*)


(*PackagePrepPackageSymbol[pkg_]:=
  With[{$Name=PackageHead[pkg], $AllowPackageSymbolDefinitions=AllowPackageSymbolDefinitions[pkg]},
    Switch[$AllowPackageSymbolDefinitions,
      None,
        If[Length@OwnValues[$Name]==0,
          Remove[$Name],
          DownValues[$Name]={}
          ],
      False,
        If[Length@OwnValues[$Name]==0,
          Clear[$Name],
          DownValues[$Name]={}
          ],
      _,
        PackageAttachMainAutocomplete[pkg]
      ]
    ]*)


(* ::Subsection:: *)
(*End*)


End[];
