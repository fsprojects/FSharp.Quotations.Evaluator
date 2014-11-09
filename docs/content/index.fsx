(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
F# Quotations Evaluator
===================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The F# Quotations Evaluator library can be <a href="https://nuget.org/packages/FSharp.Quotations.Evaluator">installed from NuGet</a>:
      <pre>PM> Install-Package FSharp.Quotations.Evaluator</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Overview
-------

This component is an F# quotations evaluator, implemented by compiling to LINQ expression trees.

For simple expression-based scenarios you may be able to simply use the method
```Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation``` from ```FSharp.Core.dll``.

However that component has restrictions in the quotations accepted - for example, statements such as
while-loops are not accepted.  This component accepts more quotations (including those involving statements), 
though some restrictions remain. Please help by contributing additional
functionality to lift remaining restrictions.

Performance of generated code is generally OK though not as good as F# compiled code, expecially for recursive functions.  
However it is still good enough for many purposes of dynamic code generation.  Contributions to improve performance
are welcome.

The component requires .NET 4.x.  It is not usable on mobile devices or portable profiles, 
where code generation APIs are not available.

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "FSharp.Quotations.Evaluator.dll"
open FSharp.Quotations.Evaluator

QuotationEvaluator.Evaluate <@ 1 + 1 @>

let addPlusOne = QuotationEvaluator.Compile <@ fun x y -> x + y + 1 @> 

let nine = addPlusOne 3 5  // gives 9


(**

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include a tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/FSharp.Quotations.Evaluator/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharp.Quotations.Evaluator
  [issues]: https://github.com/fsprojects/FSharp.Quotations.Evaluator/issues
  [readme]: https://github.com/fsprojects/FSharp.Quotations.Evaluator/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharp.Quotations.Evaluator/blob/master/LICENSE.txt
*)
