module FSharp.Quotations.Evaluator.Tests

open FSharp.Quotations.Evaluator
open NUnit.Framework

[<Test>]
let ``hello returns 42`` () =
  let result = Library.hello 42
  printfn "%i" result
  Assert.AreEqual(42,result)
