module FSharp.Quotations.Evaluator.Rosetta

open System
open System.Diagnostics
open System.Reflection
open System.Linq.Expressions
open NUnit.Framework
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator.Unittests

open Microsoft.FSharp.Quotations

type TestIterationsAttribute (count) =
    inherit Attribute ()

    member __.Count = count

    static member DefaultCount = 10000

let getTypedReflectedDefinition (functionQuotation:Expr<'t>) : MethodInfo * Expr<'t> =
    match functionQuotation with
    | Patterns.Lambda(_,Patterns.Call(_,``method``,_)) ->
        match Expr.TryGetReflectedDefinition ``method`` with
        | None -> failwith "Badness"
        | Some t -> ``method``, Expr.Cast t
    | _ -> failwith "Unexpected Form"

let getEntryPoints (functionQuotation:Expr<unit->'t>) =
    let ``method``, definition = getTypedReflectedDefinition functionQuotation

    let linqCompiledMethod = definition.Compile ()
    let directlyCallMethod = Expression.Lambda<Func<'t>>(Expression.Call(``method``)).Compile ()

    ``method``, linqCompiledMethod, directlyCallMethod

let testFunction functionQuotation =
    let ``method``, compiledMethod, directlyCallMethod =
        getEntryPoints functionQuotation

    let viaLinqCompilation = compiledMethod ()
    let directlyCalled = directlyCallMethod.Invoke ()

    check ``method``.Name viaLinqCompilation directlyCalled

let getIterations (``method``:MethodInfo) =
    let testIterations = ``method``.GetCustomAttribute<TestIterationsAttribute> ()
    if box testIterations = null
        then TestIterationsAttribute.DefaultCount
        else testIterations.Count

let timeFunction functionQuotation =
    let ``method``, compiledMethod, directlyCallMethod =
        getEntryPoints functionQuotation

    compiledMethod () |> ignore
    directlyCallMethod.Invoke () |> ignore

    let iterations = getIterations ``method``

    let sw = Stopwatch.StartNew ()
    for i=0 to iterations-1 do
        compiledMethod () |> ignore
    let viaLinqMs = sw.ElapsedMilliseconds

    let sw = Stopwatch.StartNew ()
    for i=0 to iterations-1 do
        directlyCallMethod.Invoke () |> ignore
    let directMs = sw.ElapsedMilliseconds

    Assert.LessOrEqual (viaLinqMs, 2L * directMs)

[<ReflectedDefinition; TestIterations 1000>]
let ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    let ToggleNth n (lst:bool array) =                  // Toggle every n'th door
        [(n-1) .. n .. 99]                              // For each appropriate door
        |> Seq.iter (fun i -> lst.[i] <- not lst.[i])   // toggle it
    let doors = Array.create 100 false                  // Initialize all doors to closed
    Seq.iter (fun n -> ToggleNth n doors) [1..100]      // toggle the appropriate doors for each pass
    doors 

[<Test>]
let ``Test [answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    testFunction <@ ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` @>

[<Test>]
let ``Time [answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    timeFunction <@ ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` @>

[<ReflectedDefinition>]
let ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    let PerfectSquare n =
        let sqrt = int(Math.Sqrt(float n))
        n = sqrt * sqrt
    [| for i in 1..100 do yield PerfectSquare i |]    

[<Test>]
let ``Test [answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    testFunction <@ ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` @>

[<Test>]
let ``Time [answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    timeFunction <@ ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` @>

[<ReflectedDefinition>]
let ``[Euler_method](http://rosettacode.org/wiki/Euler_method#F.23)`` () =
    let euler f (h : float) t0 y0 =
        (t0, y0)
        |> Seq.unfold (fun (t, y) -> Some((t,y), ((t + h), (y + h * (f t y)))))
 
    let newtonCoolíng (_:float) y = -0.07 * (y - 20.0)
 
    let f  = newtonCoolíng
    let a = 0.0
    let y0 = 100.0
    let b = 100.0
    let h = 10.0
    (euler newtonCoolíng h a y0)
    |> Seq.takeWhile (fun (t,_) -> t <= b)
    |> Seq.toList

[<Test>]
let ``Test [Euler_method](http://rosettacode.org/wiki/Euler_method#F.23)`` () =
    testFunction <@ ``[Euler_method](http://rosettacode.org/wiki/Euler_method#F.23)`` @>

[<Test>]
let ``Time [Euler_method](http://rosettacode.org/wiki/Euler_method#F.23)`` () =
    timeFunction <@ ``[Euler_method](http://rosettacode.org/wiki/Euler_method#F.23)`` @>


(*
[<ReflectedDefinition>]
let ``[]()`` () =

[<Test>]
let ```` () =
    testFunction <@ ```` @>
*)