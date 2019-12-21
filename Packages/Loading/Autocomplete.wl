(* ::Package:: *)

(* ::Section:: *)
(*Autocompletion*)


(* ::Text:: *)
(*Provides support for the Autocompletion mechanism exposed by the FE*)


PackageAddAutocompletions::usage=
  "PackageAddAutocompletions[pats] creates autocompletions for the specified patterns";
PackageSetAutocompletionData::usage=
  "PackageSetAutocompletionData[] configures the AutocompletionData path if autocompletions were provided for the package";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*Formats*)


$PackageAutoCompletionFormats=
  Alternatives@@Join@@{
    Range[0,9],
    {
      _String?(FileExtension[#]==="trie"&),
      {
        _String|(Alternatives@@Range[0,9])|{__String},
        (("URI"|"DependsOnArgument")->_)...
        },
      {
        _String|(Alternatives@@Range[0,9])|{__String},
        (("URI"|"DependsOnArgument")->_)...,
        (_String|(Alternatives@@Range[0,9])|{__String})
        },
      {
        __String
        }
      },
    {
      "codingNoteFontCom",
      "ConvertersPath",
      "ExternalDataCharacterEncoding",
      "MenuListCellTags",
      "MenuListConvertFormatTypes",
      "MenuListDisplayAsFormatTypes",
      "MenuListFonts",
      "MenuListGlobalEvaluators",
      "MenuListHelpWindows",
      "MenuListNotebookEvaluators",
      "MenuListNotebooksMenu",
      "MenuListPackageWindows",
      "MenuListPalettesMenu",
      "MenuListPaletteWindows",
      "MenuListPlayerWindows",
      "MenuListPrintingStyleEnvironments",
      "MenuListQuitEvaluators",
      "MenuListScreenStyleEnvironments",
      "MenuListStartEvaluators",
      "MenuListStyleDefinitions",
      "MenuListStyles",
      "MenuListStylesheetWindows",
      "MenuListTextWindows",
      "MenuListWindows",
      "PrintingStyleEnvironment",
      "ScreenStyleEnvironment",
      "Style"
      }
    };


(* ::Subsubsection:: *)
(*AddAutocompletions Base*)


PackageAddAutocompletions::doc="
Adds autocompletions using the FEPrivate`AddSpecialArgCompletion mechanism
The allowed formats are in $PackageAutoCompletionFormats and the form should be:
PackageAddAutocompletions@{
	\"FunctionName\"->fmt
	...
	}
";
PackageAddAutocompletions[
  pats:{(_String->{$PackageAutoCompletionFormats..})..}
  ]/;TrueQ[$AllowPackageAutocompletions]:=
  If[$Notebooks&&
    Internal`CachedSystemInformation["FrontEnd","VersionNumber"]>10.0,
    FrontEndExecute@FrontEnd`Value@
      Map[
        FEPrivate`AddSpecialArgCompletion[#]&,
        pats
        ],
    $Failed
    ];
PackageAddAutocompletions[
  pat:(_String->{$PackageAutoCompletionFormats..})
  ]/;TrueQ[$AllowPackageAutocompletions]:=
  PackageAddAutocompletions[{pat}];


(* ::Subsubsection::Closed:: *)
(*AddAutocompletions Helpers*)


$PackageAutocompletionAliases=
  {
    "None"|None|Normal->0,
    "AbsoluteFileName"|AbsoluteFileName->2,
    "FileName"|File|FileName->3,
    "Color"|RGBColor|Hue|XYZColor->4,
    "Package"|Package->7,
    "Directory"|Directory->8,
    "Interpreter"|Interpreter->9,
    "Notebook"|Notebook->"MenuListNotebooksMenu",
    "StyleSheet"->"MenuListStylesheetMenu",
    "Palette"->"MenuListPalettesMenu",
    "Evaluator"|Evaluator->"MenuListGlobalEvaluators",
    "FontFamily"|FontFamily->"MenuListFonts",
    "CellTag"|CellTags->"MenuListCellTags",
    "FormatType"|FormatType->"MenuListDisplayAsFormatTypes",
    "ConvertFormatType"->"MenuListConvertFormatTypes",
    "DisplayAsFormatType"->"MenuListDisplayAsFormatTypes",
    "GlobalEvaluator"->"MenuListGlobalEvaluators",
    "HelpWindow"->"MenuListHelpWindows",
    "NotebookEvaluator"->"MenuListNotebookEvaluators",
    "PrintingStyleEnvironment"|PrintingStyleEnvironment->
      "PrintingStyleEnvironment",
    "ScreenStyleEnvironment"|ScreenStyleEnvironment->
      ScreenStyleEnvironment,
    "QuitEvaluator"->"MenuListQuitEvaluators",
    "StartEvaluator"->"MenuListStartEvaluators",
    "StyleDefinitions"|StyleDefinitions->
      "MenuListStyleDefinitions",
    "CharacterEncoding"|CharacterEncoding|
      ExternalDataCharacterEncoding->
      "ExternalDataCharacterEncoding",
    "Style"|Style->"Style",
    "Window"->"MenuListWindows"
    };


(* ::Subsubsection::Closed:: *)
(*AddAutocompletions Convenience*)


$PackageAutocompletionTable={
  f:$PackageAutoCompletionFormats:>f,
  Sequence@@$PackageAutocompletionAliases,
  s_String:>{s}
  };


PackageAddAutocompletions[o:{__Rule}]/;(
  TrueQ[$AllowPackageAutocompletions]&&!TrueQ@$recursionProtect
  ):=
  Block[{$recursionProtect=True},
    Replace[
      PackageAddAutocompletions@
        Replace[o,
          (s_->v_):>
            (Replace[s,_Symbol:>SymbolName[s]]->
              Replace[
                Flatten[{v},1],
                $PackageAutocompletionTable,
                1
                ]),
          1
          ],
      _PackageAddAutocompletions->$Failed
      ]
    ];
PackageAddAutocompletions[s:Except[_List],v_]/;TrueQ[$AllowPackageAutocompletions]:=
  PackageAddAutocompletions[{s->v}];
PackageAddAutocompletions[l_, v_]/;TrueQ[$AllowPackageAutocompletions]:=
  PackageAddAutocompletions@
    Flatten@{
      Quiet@
        Check[
          Thread[l->v],
          Map[l->#&,v]
          ]
      };


(* ::Subsubsection::Closed:: *)
(* PackageSetAutocompletionData *)


PackageSetAutocompletionData[]:=
  If[DirectoryQ@
      FileNameJoin@{
          $PackageDirectory,
          "Resources",
          "FunctionalFrequency"
          },
    CurrentValue[
      $FrontEndSession,
      {PrivatePaths,"AutoCompletionData"}
      ]=
      DeleteDuplicates@
        Append[
          CurrentValue[
            $FrontEndSession,
            {PrivatePaths,"AutoCompletionData"}
            ],
          FileNameJoin@{
            $PackageDirectory,
            "Resources",
            "FunctionalFrequency"
            }
          ]
    ];


(* ::Subsection:: *)
(*End*)


End[]
