module FSharp.Quotations.Evaluator.Rosetta

open System
open NUnit.Framework
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator.Unittests

open Microsoft.FSharp.Quotations

// [Expanding quotations](http://fssnip.net/bx)
let expand (expr:Expr<'a>) : Expr<'a>=
    /// The parameter 'vars' is an immutable map that assigns expressions to variables
    /// (as we recursively process the tree, we replace all known variables)
    let rec expand vars expr = 
      // First recursively process & replace variables
      let expanded = 
        match expr with
        // If the variable has an assignment, then replace it with the expression
        | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]
        // Apply 'expand' recursively on all sub-expressions
        | ExprShape.ShapeVar v -> Expr.Var v
        | Patterns.Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) ->
            let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
            let res = Expr.Applications(this, [ for a in args -> [a]])
            expand vars res
        | ExprShape.ShapeLambda(v, expr) -> 
            Expr.Lambda(v, expand vars expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            ExprShape.RebuildShapeCombination(o, List.map (expand vars) exprs)

      // After expanding, try reducing the expression - we can replace 'let'
      // expressions and applications where the first argument is lambda
      match expanded with
      | Patterns.Application(ExprShape.ShapeLambda(v, body), assign)
      | Patterns.Let(v, assign, body) ->
          expand (Map.add v (expand vars assign) vars) body
      | _ -> expanded

    match expand Map.empty expr with
    | Patterns.Lambda(v,body) -> Expr.Cast body
    | _ -> failwith "Unexpected Form"

[<ReflectedDefinition>]
let ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    let ToggleNth n (lst:bool array) =                  // Toggle every n'th door
        [(n-1) .. n .. 99]                              // For each appropriate door
        |> Seq.iter (fun i -> lst.[i] <- not lst.[i])   // toggle it
    let doors = Array.create 100 false                  // Initialize all doors to closed
    Seq.iter (fun n -> ToggleNth n doors) [1..100]      // toggle the appropriate doors for each pass
    doors 

[<ReflectedDefinition>]
let ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    let PerfectSquare n =
        let sqrt = int(Math.Sqrt(float n))
        n = sqrt * sqrt
    [| for i in 1..100 do yield PerfectSquare i |]    

[<Test>]
let ``http://rosettacode.org/wiki/100_doors#F.23`` () =
    let quotation = expand <@ ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () @>
    let result = ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` ()

    checkEval "1" quotation result

    let quotation = expand <@ ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () @>
    let result = ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` ()

    checkEval "2" quotation result
    
