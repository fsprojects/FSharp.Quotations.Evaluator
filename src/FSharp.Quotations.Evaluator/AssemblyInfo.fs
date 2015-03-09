namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Quotations.Evaluator")>]
[<assembly: AssemblyProductAttribute("FSharp.Quotations.Evaluator")>]
[<assembly: AssemblyDescriptionAttribute("A quotations evaluator for F# based on LINQ expression tree compilation")>]
[<assembly: AssemblyVersionAttribute("1.0.5")>]
[<assembly: AssemblyFileVersionAttribute("1.0.5")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.5"
