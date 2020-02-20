(* ::Package:: *)

Paclet[
  Name -> "PackageFramework",
  Version -> "1.0.0",
  Extensions -> {
    	{
     		"Kernel",
     		"Root" -> ".",
     		"Context" -> {"PackageFramework`"}
     	},
     {
       "PackageFramework",
       "AutoloadIgnored"->{
         "PackageFrameworkPackage",
         "$PackageFrameworkPackages", 
         "$PackageFrameworkVersion",
         "PackageFrameworkPackageQ",
         "PackageFrameworkPackageOptionValue",
         "PackageAutoloadPackage",
         "PackageCompleteLoadProcess",
         "PackagePreloadedPackages",
         "PackageLoadedPackages",
         "PackageLoadedPackages",
         "PackageAddLoadedPackages"
         }
      }
    }
 ]
