(* ::Package:: *)

PacletObject[
    <|
        "Name" -> "TBounce",
        "Version" -> "0.1.6",
        "IsPrereleaseVersion" -> True,
        "Description" -> "Preparing for release."(*"Run the BSM\[Rule]GW pipeline for a given temperature-dependent scalar potential V(\[Phi],T)."*),
        "WolframVersion" -> "13.+",
        "Creator" -> "Vedran Brdar, Marco Finetti, Marco Matteini, Antonio Morais, Miha Nemevsek",
        "Icon" -> "FrontEnd/icon.png",
        "Category" -> "Physics",
        "Keywords" -> {"bounce","cosmology","phase transitions","gravitational waves","thermal potential","scalar potential","BSM"},
        "Extensions" ->
            {
                {
                    "Kernel",
                    "Root" -> "Kernel",
                    "Context" -> {"TBounce`","GW`","Examples`"}
                },
                {
                    "Documentation",
                    "Language" -> "English",
                    "MainPage" -> "Guides/TBouncePackage"
                },
                {
                    "FrontEnd",
                    "Root" -> "FrontEnd"
                },
                {
                    "Path",
                    "Root" -> "Extensions"
                }
            }
    |>
]
