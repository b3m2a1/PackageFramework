(* ::Package:: *)

(* ::Section:: *)
(* Package Functions *)


(* ::Subsection:: *)
(*Post-Processing*)


PackagePostProcessPrepSpecs::usage="";
PackagePrepPackageSymbol::usage="";
PackagePostProcessExposePackages::usage="";
PackagePostProcessRehidePackages::usage="";
PackagePostProcessDecontextPackages::usage="";
PackagePostProcessContextPathReassign::usage="";
PackageAttachMainAutocomplete::usage="";
PackagePreemptShadowing::usage="";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection:: *)
(*PrepFileName*)


PackagePostProcessFileNamePrep[fn_]:=
    Replace[
      FileNameSplit@
        FileNameDrop[fn,
          FileNameDepth@
            PackageFilePath["Packages"]
          ],{
      {f_}:>
        f|fn|StringTrim[f,".m"|".wl"],
      {p__,f_}:>
        FileNameJoin@{p,f}|fn|{p,StringTrim[f,".m"|".wl"]}
      }]


(* ::Subsubsection::Closed:: *)
(*PrepSpecs*)


PackagePostProcessPrepSpecs[]:=
  (
    Unprotect[
      $PackagePreloadedPackages,
      $PackageHiddenPackages,
      $PackageHiddenContexts,
      $PackageExposedContexts,
      $PackageDecontextedPackages
      ];
    Replace[
      $PackageLoadSpecs,
      specs:{__Rule}|_Association:>
        CompoundExpression[
          $PackagePreloadedPackages=
            Replace[
              Lookup[specs, "PreLoad"],
              Except[{__String}]->{}
              ],
          $PackageHiddenPackages=
            Replace[
              Lookup[specs,"FEHidden"],
              Except[{__String}]->{}
              ],
          $PackageDecontextedPackages=
            Replace[
              Lookup[specs,"PackageScope"],
              Except[{__String}]->{}
              ],
          $PackageExposedContexts=
            Replace[
              Lookup[specs,"ExposedContexts"],
              Except[{__String}]->{}
              ]
          ]
        ]
    );


(* ::Subsubsection::Closed:: *)
(*ExposePackages*)


PackagePostProcessExposePackages[]/;TrueQ[$AllowPackageRecoloring]:=
  (
    PackageAppGet/@
      $PackagePreloadedPackages;
    With[{
      syms=
        If[
          !MemberQ[$PackageHiddenPackages,
            PackagePostProcessFileNamePrep[#]
            ],
          $DeclaredPackages[#],
          {}
          ]&/@Keys@$DeclaredPackages//Flatten
      },
      Replace[
        Thread[
          If[ListQ@$PackageFEHiddenSymbols,
            DeleteCases[syms,
              Alternatives@@
                (Verbatim[HoldPattern]/@Flatten@$PackageFEHiddenSymbols)
              ],
            syms
            ],
          HoldPattern],
        Verbatim[HoldPattern][{s__}]:>
          PackageFEUnhideSymbols[s]
        ]
      ]
    )




(* ::Subsubsection::Closed:: *)
(*Rehide Packages*)


PackagePostProcessRehidePackages[]/;TrueQ[$AllowPackageRecoloring]:=
  If[
    MemberQ[$PackageHiddenPackages,
      PackagePostProcessFileNamePrep[#]
      ],
    PackageFERehidePackage@#
    ]&/@Keys@$DeclaredPackages


(* ::Subsubsection::Closed:: *)
(*Decontext*)


PackagePostProcessDecontextPackages[]/;TrueQ[$AllowPackageRecoloring]:=
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




(* ::Subsubsection::Closed:: *)
(*ContextPathReassign*)


PackagePostProcessContextPathReassign[]:=
  With[{cp=$ContextPath},
    If[MemberQ[cp],
      "$Name`",
      $ContextPath=
        Join[
          Replace[
            Flatten@{$PackageExposedContexts},
            Except[_String?(StringEndsQ["`"])]->Nothing,
            1
            ],
          $ContextPath
          ];
      If[TrueQ[$AllowPackageRecoloring], 
        FrontEnd`Private`GetUpdatedSymbolContexts[]
        ];
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*AttachMainAutocomplete*)


PackageAttachMainAutocomplete[]:=
  PackageAddAutocompletions[$Name, 
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
    ];


(* ::Subsubsection::Closed:: *)
(*PreventShadowing*)


PackagePreemptShadowing[]:=
  Replace[
    Hold[{m___}]:>
      Off[m]
      ]@
    Thread[
      ToExpression[
        Map[#<>"$"&, Names["`PackageScope`Private`*"]
        ],
        StandardForm,
        Function[Null, 
          Hold[MessageName[#, "shdw"]],
          HoldAllComplete
          ]
        ],
      Hold
      ]


(* ::Subsubsection:: *)
(*PackagePrepPackageSymbol*)


PackagePrepPackageSymbol[]:=
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
      PackageAttachMainAutocomplete[]
    ]


(* ::Subsection:: *)
(*End*)


End[];
