(* ::Package:: *)

(* ::Section:: *)
(*Package*)


(* ::Text:: *)
(*There are some aspects of the package that provide an object oriented interface to the system. These are handled in this sub package file.*)


$PackageFrameworkPackages::usage=
  "$PackageFrameworkPackages is a manager symbol so that can avoid a reload if unnecessary";
$CurrentPackage::usage="The current package as configured by the loader";
$PackageFrameworkVersion::usage="The version of the PackageFramework in play";
ResolvePackageFrameworkPackage::usage="Resolves to the unique package object if needed";


PackageCreateCurrentPackageOptionInterface::usage=
  "Implementation function that lets a symbol become a hook into a given attribute of a package";
BindPackageMethod::usage=
  "Adds a method on the general PackageFrameworkPackage interface that makes it possible to call\
 into packages";
PackageFrameworkConfig::usage="Discovers the config settings for a given package / directory";
PackageFrameworkPackageQ::usage="Tests whether the object is an initialized package for not";
PackageFrameworkPackageOption::usage="Gets an option value for a package";
PackageFrameworkPackageOptionValue::usage="Gets an option value for a package";
PackageFrameworkPackageMutate::usage="The mutation handler for PackageFrameworkPackage objects";


(* ::Subsection:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*$PackageFrameworkVersion*)


$PackageFrameworkVersion::doc="
Dunno if I'll ever use this, but this should either be standalone-version or paclet-version \
where version is semantically versioned.

The idea will be to allow for more dynamic loading of the PackageFramework (potentially), maybe using \
some helper function
";
$PackageFrameworkVersion="paclet-0.0.0";


(* ::Subsubsection::Closed:: *)
(*$PackageFrameworkPackages*)


If[!ValueQ[$PackageFrameworkPackages],
  $PackageFrameworkPackages = <||>
  ];


(* ::Subsubsection::Closed:: *)
(*ResolvePackageFrameworkPackage*)


ResolvePackageFrameworkPackage[loc_, parent_:None]:=
  If[parent===None,
    Lookup[$PackageFrameworkPackages, loc, None],
    Lookup[Lookup[$PackageFrameworkPackages, parent, <||>], loc, None]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkConfig*)


$PacletInfoFileNames={
  "PacletInfo.wl", "PacletInfo.m"
  };


$LegacyInfoFileNames={
  "LoadInfo.m", "LoadInfo.wl",
  "BundleInfo.m", "BundleInfo.wl"
  };


PackageFrameworkConfig::doc="
	Makes it possible to support the legacy design from when this was all part of BTools
and I expected a Config folder at the root of every package

The new design attempts to unify my two major use cases: 
	1) making new packages with my standard layout and which will auto-context for you
	2) hooking into existing packages for extension and introspection
";
PackageFrameworkConfig[ops_, paclet_, rootDirectory_]:=
  Module[
    {
      pacletData = paclet, 
      legacySettings = {},
      legacyConfigFiles,
      extensionData
      },
     Switch[pacletData,
       None,
         (* proceed to try to find the missing PacletInfo.m or PacletInfo.wl file *)
         pacletData = 
           Catch[
             Do[
               If[FileExistsQ[FileNameJoin@{rootDirectory, p}],
                 Throw[
                   Association@@PacletManager`CreatePaclet@
                     FileNameJoin@{rootDirectory, p}
                   ]
                 ],
               {p, $PacletInfoFileNames}
               ];
             <||>
             ],
       _PacletManager`Paclet|_System`PacletObject,
         pacletData = Association@@pacletData
       ];
    extensionData=
      Association@
        Replace[
          Lookup[pacletData, "Extensions", {}],
          {key_String, o___}:>
            (key->Association@Flatten@{o}),
          1
          ];
    legacyConfigFiles=
      FileNames[
        Alternatives@@$LegacyInfoFileNames, 
        {
          FileNameJoin@{rootDirectory, "Config"},
          FileNameJoin@{rootDirectory, "Private"},
          FileNameJoin@{rootDirectory, "Private", "Config"}
          }
        ];
    legacySettings=
      Flatten@Map[
        Replace[
          Quiet[
            Import@#,
            {
              Import::nffil,
              Import::chtype
              }
            ],
          Except[KeyValuePattern[{}]]:>
            {}
          ]&,
       legacyConfigFiles
       ];
    Join[
       <|
         "Name"->Lookup[pacletData, "Name"],
         "Location"->Lookup[pacletData, "Location"],
         "Context"->Replace[
           Lookup[Lookup[pacletData, "Kernel", <||>], "Context"],
           {
             m_Missing:>{FileBaseName@rootDirectory<>"`"},
             e_:>Flatten@{e}
             }
           ]
         |>,
       Lookup[extensionData, "PackageFramework", <||>]
       ]//Merge[
       Flatten@{
         ops,
         #,
         legacySettings,
         Options[PackageFrameworkPackage]
         },
       Replace[{
         r:{(Except[{}, _?OptionQ]|_Association)..}:>Merge[r, First],
         {e_, ___}:>e
         }]
       ]&
    ]


(* ::Subsubsection::Closed:: *)
(*Formatting*)


MakeBoxes[p:PackageFrameworkPackage[a_]?PackageFrameworkPackageQ, StandardForm]:=
  BoxForm`ArrangeSummaryBox[
    PackageFrameworkPackage,
    p,
    None,
    {
      BoxForm`MakeSummaryItem[{"Name: ", a["Name"]}, StandardForm]
      },
    { 
      BoxForm`MakeSummaryItem[{"Context: ", a["Context"]}, StandardForm],
      BoxForm`MakeSummaryItem[{"Location: ", a["Location"]}, StandardForm]
      },
    StandardForm
    ]


(* ::Subsubsection::Closed:: *)
(*ConstructPackageFrameworkPackage*)


ConstructPackageFrameworkPackage//Clear
ConstructPackageFrameworkPackage[
  pkg:_PacletManager`Paclet|_System`PacletObject,
  ops:OptionsPattern[]
  ]:=
  With[{loc=pkg["Location"]},
    If[StringQ@loc&&DirectoryQ@loc,
      Replace[
        ResolvePackageFrameworkPackage[loc],
        None:>
          ConstructPackageFrameworkPackage[
            PackageFrameworkConfig[{ops}, pkg, loc],
            True
            ]
        ],
      $Failed
      ]
    ];
ConstructPackageFrameworkPackage[
  loc:_String?DirectoryQ,
  ops:OptionsPattern[]
  ]:=
  Replace[
    ResolvePackageFrameworkPackage[loc],
    None:>
      ConstructPackageFrameworkPackage[
        PackageFrameworkConfig[{ops}, None, loc],
        True
        ]
    ];
ConstructPackageFrameworkPackage[
  ops:OptionsPattern[]
  ]:=
  With[{loc=OptionValue["Location"]},
    If[StringQ[loc]&&DirectoryQ[loc],
      Replace[
        ResolvePackageFrameworkPackage[loc],
        None:>
          ConstructPackageFrameworkPackage[
            PackageFrameworkConfig[{ops}, None, loc],
            True
            ]
        ],
      $Failed
      ]
    ];
ConstructPackageFrameworkPackage[a_Association, validate_:True]:=
  If[validate,
    With[{loc=a["Location"], cont=First@Flatten@{a["Context"]}},
      If[StringQ[loc]&&DirectoryQ[loc],
        Replace[
          ResolvePackageFrameworkPackage[loc],
          None:>
            Replace[
              ResolvePackageFrameworkPackage[cont],
              None:>
                With[{config=PackageFrameworkConfig[Normal[a], None, loc]},
                  ConstructPackageFrameworkPackage[a, False]
                  ]
              ]
          ],
        $Failed
        ]
      ],
   System`Private`SetValid@Unevaluated@PackageFrameworkPackage[a]
   ]


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageQ*)


PackageFrameworkPackageQ[p:PackageFrameworkPackage[_Association?AssociationQ]]:=
  System`Private`ValidQ[p]
PackageFrameworkPackageQ[_]:=False


(* ::Subsubsection::Closed:: *)
(*PropertyValue / SetProperty / RemoveProperty / PropertyList*)


If[!ValueQ@$PackagePropertyStore,
  $PackagePropertyStore = Language`NewExpressionStore["<PackageProperties>"];
  ];


(* ::Subsubsubsubsection:: *)
(*PPSPropContainsQ*)


PPSPropContainsQ[x_]:=
	$PackagePropertyStore@"containsQ"[x];
PPSPropContainsQ[x_, p_]:=
	$PackagePropertyStore@"containsQ"[x, p];


(* ::Subsubsubsubsection:: *)
(*PPSPropGet*)


PPSPropGet[x_, p_String]:=
	$PackagePropertyStore@"get"[x, p];
PPSPropGet[x_, {p_String}]:=
  PPSPropGet[x, p];


(* ::Subsubsubsubsection:: *)
(*PPSPropSet*)


PPSPropSet[x_, p_, v_]:=
	$PackagePropertyStore@"put"[x, p ,v];


(* ::Subsubsubsubsection:: *)
(*PPSPropRemove*)


PPSPropRemove[x_]:=
	$PackagePropertyStore@"remove"[x];
PPSPropRemove[x_, p_]:=
	$PackagePropertyStore@"remove"[x, p];


(* ::Subsubsubsubsection:: *)
(*PPSPropKeys*)


PPSPropKeys[]:=
	$PackagePropertyStore@"getKeys"[];
PPSPropKeys[x_]:=
	$PackagePropertyStore@"getKeys"[x];


(* ::Subsubsubsubsection:: *)
(*PPSPropList*)


PPSPropList[]:=
	$PackagePropertyStore@"listTable"[];


(* ::Subsubsubsubsection:: *)
(*PPSCopyProperties*)


PPSCopyProperties[a_, b_]:=
  Do[PPSPropSet[b, PPSPropGet[a, k]], {k, PPSPropKeys[a]}]


(* ::Subsubsubsubsection:: *)
(*SetProperty*)


$$hold~SetAttributes~HoldAllComplete;
PackageFrameworkPackageSetProperty[pkg_, p_->v_]:=
  PPSPropSet[pkg, p, v];
PackageFrameworkPackageSetProperty[pkg_, p_:>v_]:=
  PPSPropSet[pkg, p->$$hold[v]];
PackageFrameworkPackageSetProperty[pkg_, {p_String}->v_]:=
  PPSPropSet[pkg, p, v];
PackageFrameworkPackageSetProperty[pkg_, {p_String, r__}->v_]:=
  Module[{a=PPSPropGet[pkg, p]},
    a[[r]]=v;
    PackageFrameworkPackageSetProperty[pkg, p->a];
    ];


PackageFrameworkPackage/:
  SetProperty[pkg_PackageFrameworkPackage, p_->v_]:=
	  PPSPropSet[pkg, p, v];
PackageFrameworkPackage/:
  SetProperty[pkg_PackageFrameworkPackage, p_:>v_]:=
	  PPSPropSet[pkg, p, v]


(* ::Subsubsubsubsection:: *)
(*PropertyValue*)


PackageFrameworkPackagePropertyValue[pkg_, p_]:=
  Replace[
  	  PPSPropGet[pkg, p],
  		{
  			Null:>If[!PPSPropContainsQ[pkg, p], Missing["PropertyAbsent", p], Null],
  			$$hold[v_]:>v
  			}
  		];
PackageFrameworkPackagePropertyValue[pkg_, {p_String}]:=
  PackageFrameworkPackagePropertyValue[pkg, p];
PackageFrameworkPackagePropertyValue[pkg_, {p_String, r__}]:=
  With[{a=PackageFrameworkPackagePropertyValue[pkg, p]},
    If[a=!=Missing["PropertyAbsent", p],
      a[[r]],
      a
      ]
    ]


PackageFrameworkPackage/:
  PropertyValue[pkg_PackageFrameworkPackage, p_]:=
  	PackageFrameworkPackagePropertyValue[pkg, p]


(* ::Subsubsubsubsection:: *)
(*RemoveProperty*)


PackageFrameworkPackageRemoveProperty[pkg_PackageFrameworkPackage, All]:=
  Map[
    PackageFrameworkPackageRemoveProperty[pkg, #]&,
    PackageFrameworkPackagePropertyList[pkg]
    ];
PackageFrameworkPackageRemoveProperty[pkg_PackageFrameworkPackage, p_]:=
  Replace[
    PPSPropRemove[pkg, p],
    {
      $$hold[v_]:>v
      }
    ];
PackageFrameworkPackageRemoveProperty[pkg_PackageFrameworkPackage, {p_String, ___}]:=
  PackageFrameworkPackageRemoveProperty[pkg, p]


PackageFrameworkPackage/:
  RemoveProperty[pkg_PackageFrameworkPackage, p_]:=
    PackageFrameworkPackageRemoveProperty[pkg, p];


(* ::Subsubsubsubsection:: *)
(*PropertyList*)


PackageFrameworkPackagePropertyList[pkg_]:=
  Replace[PPSPropKeys[pkg], Null->Missing["PackageAbsent", pkg]]


PackageFrameworkPackage/:
  PropertyList[pkg_PackageFrameworkPackage]:=
    PackageFrameworkPackagePropertyList[pkg];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageOptionValue*)


PackageFrameworkPackageOptionValue[
  pkg:PackageFrameworkPackage[a_Association]?PackageFrameworkPackageQ, 
  keys:__String
  ]:=
  Fold[Lookup[#, #2, Return[Missing["KeyAbsent", #2], Fold]]&, a, {keys}];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageSetOptions*)


PackageFrameworkPackageSetOptions[
  pkg:PackageFrameworkPackage[a_Association]?PackageFrameworkPackageQ, 
  ops:(
    (Rule|RuleDelayed)[_String|{__String}, _]|
    {
      (Rule|RuleDelayed)[_String|{__String}, _]..
      }
    )
  ]:=
  With[{aa=ReplacePart[a, ops]},
    With[{new=System`Private`SetValid@Unevaluated@PackageFrameworkPackage[aa]},
      PPSCopyProperties[pkg, new];
      new
      ]
    ]


p_PackageFrameworkPackage?PackageFrameworkPackageQ[keys:__String]:=
  PackageFrameworkPackageOptionValue[p, keys];
p_PackageFrameworkPackage?PackageFrameworkPackageQ[keys:__String]:=
  PackageFrameworkPackageOptionValue[p, keys];
PackageFrameworkPackage/:
  (Set[pkg_PackageFrameworkPackage?PackageFrameworkPackageQ[keys:__String], value_]):=
    PackageFrameworkPackageSetOptions[pkg, {keys}->value];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageMutate*)


PackageFrameworkPackageMutate//ClearAll
PackageFrameworkPackageMutate~SetAttributes~HoldAllComplete
PackageFrameworkPackageMutate[
  s_Symbol?PackageFrameworkPackageQ[keys:__String]=value_
  ]:=
  (s=PackageFrameworkPackageSetOptions[s, {keys}->value]);
PackageFrameworkPackageMutate[___]:=Language`MutationFallthrough


(* ::Subsubsection:: *)
(*CreatePackageSymbolInterface*)


(* ::Text:: *)
(*The name of the method is stupidly long, of course, but the basic idea is to make it possible to work really easily with the $CurrentPackage since that's the only one we'll ever really care about at any given moment in time*)
(**)
(*We'll provide an accessor method that can be bound dynamically to the value of the symbol so that it can be applied to $CurrentPackage and then we'll also provide a normal setter-interface that will have to work through SetProperty and friends...I think. Or I guess that could be a default too.*)


SetAttributes[CreatePackageSymbolInterface, HoldFirst];
CreatePackageSymbolInterface::doc="
Binds the PropertyValue/SetProperty interface to the specified symbol and \
uses the specified string as the key.
";
CreatePackageSymbolInterface[
  symbol_, 
  setting_String,
  accessor_:None,
  setter_:None
  ]:=(
  Unprotect[Unevaluated@symbol];
  ClearAll[Unevaluated@symbol];
  With[
    {
      a = If[accessor === None, PropertyValue, accessor],
      s = If[setter === None, SetProperty, setter]
      },
    symbol := a[$CurrentPackage, setting];
    symbol /: Set[symbol, value_]:=
      s[$CurrentPackage, setting->value];
    symbol /: Set[symbol[k__], value_]:=
      s[$CurrentPackage, {setting, k}->value];
    symbol /: AppendTo[symbol, value_]:=
      s[$CurrentPackage, setting->Append[a[$CurrentPackage], value]];
    symbol /: AssociateTo[symbol, key_->value_]:=
      s[$CurrentPackage, {setting, key}->value];
    ];
  Protect[symbol];
  );


(* ::Subsubsection::Closed:: *)
(*BindPackageMethod*)


(* ::Text:: *)
(*Just makes it so that the PackageFrameworkPackage can make use of down-value type methods. We might want to add to it later, but for the moment it makes it very clean and clear to work with*)


BindPackageMethod[name_, method_]:=
  (
    (pkg:_PackageFrameworkPackage?PackageFrameworkPackageQ)@name[args___]:=method[pkg, args]
    )


(* ::Subsubsection::Closed:: *)
(*$CurrentPackage*)


If[!ValueQ[$CurrentPackage],
  $CurrentPackage = None;
  Protect[$CurrentPackage]
  ]


(* ::Subsection::Closed:: *)
(*End Private*)


End[];
