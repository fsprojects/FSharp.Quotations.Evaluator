module FSharp.Quotations.Evaluator.Performance

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

type TimeAllowanceAttribute (multiplier) =
    inherit Attribute ()
    member __.Multiplier = multiplier
    static member DefaultMultiplier = 2.0

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

let getTimeAllowanceMultiplier (``method``:MethodInfo) =
    let timeAllowance = ``method``.GetCustomAttribute<TimeAllowanceAttribute> ()
    if box timeAllowance = null
        then TimeAllowanceAttribute.DefaultMultiplier
        else timeAllowance.Multiplier

#if INCLUDE_TIMING_TESTS
// we probably have a lot of ceremony here for no particular reason, but it shouldn't hurt
let timeFunction functionQuotation =
    let ``method``, compiledMethod, directlyCallMethod =
        getEntryPoints functionQuotation

    // just run them once to 'prime' them to some degree... 
    compiledMethod ()            |> ignore
    directlyCallMethod.Invoke () |> ignore

    let iterations              = ``method`` |> getIterations
    let timeAllowanceMultiplier = ``method`` |> getTimeAllowanceMultiplier

    let getLinqCompiledTime () =
        GC.Collect ()
        System.Threading.Thread.Sleep(1) // start a time slice afresh; maybe!
        let sw = Stopwatch.StartNew ()
        for i=0 to iterations-1 do
            compiledMethod () |> ignore
        float sw.ElapsedMilliseconds

    let getDirectelyCompiledTime () =
        GC.Collect ()
        System.Threading.Thread.Sleep(1) // start a time slice afresh; maybe!
        let sw = Stopwatch.StartNew ()
        for i=0 to iterations-1 do
            directlyCallMethod.Invoke () |> ignore
        float sw.ElapsedMilliseconds

    let directMs, viaLinqMs = 
        // "randomly" choose one to run first
        if DateTime.Now.Millisecond / 100 % 2 = 1 then
            getDirectelyCompiledTime(), getLinqCompiledTime ()
        else
            let linqCompiledTime      = getLinqCompiledTime ()
            let directelyCompiledTime = getDirectelyCompiledTime () 
            directelyCompiledTime, linqCompiledTime

    let allowedTime = directMs * timeAllowanceMultiplier

    Assert.LessOrEqual (viaLinqMs, allowedTime,
        "Took too long! linq={0:0} compiled={1:0} multiples of allowed time={2:0.00}",
        viaLinqMs, directMs, viaLinqMs / allowedTime)

    Assert.GreaterOrEqual (viaLinqMs, allowedTime * 0.75,
        "Too fast; decrease the multiplier! linq={0:0} compiled={1:0} allowed multiples={2:0.00}-{3:0.00} actual multiples={4:0.00}", 
        viaLinqMs, directMs, timeAllowanceMultiplier * 0.75, timeAllowanceMultiplier, viaLinqMs / directMs)
#else
let timeFunction _ =
    Assert.Ignore "Ignoring timing tests. Set INCLUDE_TIMING_TESTS"
#endif

[<ReflectedDefinition; TestIterations 1000; TimeAllowance 4.2>]
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


[<ReflectedDefinition; TimeAllowance 1.6>]
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


[<ReflectedDefinition; TimeAllowance 1.7>]
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


[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.1>]
let ``int Operators +-/*%`` () =
    let rand = Random 3141592
    let mutable result = 0
    for i = 1 to 10000 do
        let mutable x = rand.Next ()
        x <- x / i
        x <- x + i
        x <- x * i
        x <- x - i 
        if x % 2 = 1 then
            result <- result + 1
    result

[<Test>]
let ``Test int Operators +-/*%`` () =
    testFunction <@ ``int Operators +-/*%`` @>

[<Test>]
let ``Time int Operators +-/*%`` () =
    timeFunction <@ ``int Operators +-/*%`` @>


[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.2>]
let ``int64 Operators +-/*%`` () =
    let rand = Random 3141592
    let mutable result = 0L
    for i = 1 to 10000 do
        let mutable x = int64 <| rand.Next ()
        let i = int64 i
        x <- x / i
        x <- x + i
        x <- x * i
        x <- x - i 
        if x % 2L = 1L then
            result <- result + 1L
    result

[<Test>]
let ``Test int64 Operators +-/*%`` () =
    testFunction <@ ``int64 Operators +-/*%`` @>

[<Test>]
let ``Time int64 Operators +-/*%`` () =
    timeFunction <@ ``int64 Operators +-/*%`` @>


[<ReflectedDefinition; TestIterations 100; TimeAllowance 1.2>]
let ``float Operators +-/*%`` () =
    let rand = Random 3141592
    let mutable result = 0.0
    for i = 1 to 10000 do
        let mutable x = float <| rand.Next ()
        let i = float i
        x <- x / i
        x <- x + i
        x <- x * i
        x <- x - i 
        if x % 2.0 = 1.0 then
            result <- result + 1.0
    result

[<Test>]
let ``Test float Operators +-/*%`` () =
    testFunction <@ ``float Operators +-/*%`` @>

[<Test>]
let ``Time float Operators +-/*%`` () =
    timeFunction <@ ``float Operators +-/*%`` @>

(*
fails to run compiled version; I think because of Microsoft.FSharp.Core.ExtraTopLevelOperators.ToSingle

[<ReflectedDefinition; TestIterations 100; TimeAllowance 30.0>]
let ``single Operators +-/*%`` () =
    let rand = Random 3141592
    let mutable result = 0.0
    for i = 1 to 10000 do
        let mutable x = single <| rand.Next ()
        let i = single i
        x <- x / i
        x <- x + i
        x <- x * i
        x <- x - i 
        if x % 2.0f = 1.0f then
            result <- result + 1.0
    result

[<Test>]
let ``Test single Operators +-/*%`` () =
    testFunction <@ ``single Operators +-/*%`` @>

[<Test>]
let ``Time single Operators +-/*%`` () =
    timeFunction <@ ``single Operators +-/*%`` @>
*)


[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.1>]
let ``int Operators <>=`` () =
    let rand = Random 3141592
    let mutable result = 0
    for i = 1 to 1000 do
        let a = rand.Next ()
        let b = rand.Next ()
        let c = rand.Next ()
        let d = rand.Next ()
        let e = rand.Next 3
        let f = rand.Next 3
        let g = rand.Next 3
        if a < b  then result <- result + 1
        if b > c  then result <- result - 1
        if a >= c then result <- result + 2
        if a <= d then result <- result - 2
        if e = f  then result <- result + 3
        if f <> g then result <- result - 3
    result

[<Test>]
let ``Test int Operators <>=`` () =
    testFunction <@ ``int Operators <>=`` @>

[<Test>]
let ``Time int Operators <>=`` () =
    timeFunction <@ ``int Operators <>=`` @>


[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.1>]
let ``int64 Operators <>=`` () =
    let rand = Random 3141592
    let mutable result = 0
    for i = 1 to 1000 do
        let a = rand.Next ()
        let b = rand.Next ()
        let c = rand.Next ()
        let d = rand.Next ()
        let e = rand.Next 3
        let f = rand.Next 3
        let g = rand.Next 3
        if a < b  then result <- result + 1
        if b > c  then result <- result - 1
        if a >= c then result <- result + 2
        if a <= d then result <- result - 2
        if e = f  then result <- result + 3
        if f <> g then result <- result - 3
    result

[<Test>]
let ``Test int64 Operators <>=`` () =
    testFunction <@ ``int64 Operators <>=`` @>

[<Test>]
let ``Time int64 Operators <>=`` () =
    timeFunction <@ ``int64 Operators <>=`` @>


[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.1>]
let ``float Operators <>=`` () =
    let rand = Random 3141592
    let mutable result = 0
    for i = 1 to 1000 do
        let a = rand.Next ()
        let b = rand.Next ()
        let c = rand.Next ()
        let d = rand.Next ()
        let e = rand.Next 3
        let f = rand.Next 3
        let g = rand.Next 3
        if a < b  then result <- result + 1
        if b > c  then result <- result - 1
        if a >= c then result <- result + 2
        if a <= d then result <- result - 2
        if e = f  then result <- result + 3
        if f <> g then result <- result - 3
    result

[<Test>]
let ``Test float Operators <>=`` () =
    testFunction <@ ``float Operators <>=`` @>

[<Test>]
let ``Time float Operators <>=`` () =
    timeFunction <@ ``float Operators <>=`` @>


[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.1>]
let ``single Operators <>=`` () =
    let rand = Random 3141592
    let mutable result = 0
    for i = 1 to 1000 do
        let a = rand.Next ()
        let b = rand.Next ()
        let c = rand.Next ()
        let d = rand.Next ()
        let e = rand.Next 3
        let f = rand.Next 3
        let g = rand.Next 3
        if a < b  then result <- result + 1
        if b > c  then result <- result - 1
        if a >= c then result <- result + 2
        if a <= d then result <- result - 2
        if e = f  then result <- result + 3
        if f <> g then result <- result - 3
    result

[<Test>]
let ``Test single Operators <>=`` () =
    testFunction <@ ``single Operators <>=`` @>

[<Test>]
let ``Time single Operators <>=`` () =
    timeFunction <@ ``single Operators <>=`` @>


[<ReflectedDefinition; TimeAllowance 1.2>]
let ``float cast`` () =
    let rand = Random 3141592
    let mutable total = 0.0
    for i=0 to 1000 do
        total <- total + (float (rand.Next ()))
    total

[<Test>]
let ``Test float cast`` () =
    testFunction <@ ``float cast`` @>

[<Test>]
let ``Time float cast`` () =
    timeFunction <@ ``float cast`` @>


[<ReflectedDefinition; TimeAllowance 1.2>]
let ``int64 cast`` () =
    let rand = Random 3141592
    let mutable total = 0L
    for i=0 to 1000 do
        total <- total + (int64 (rand.Next ()))
    total

[<Test>]
let ``Test int64 cast`` () =
    testFunction <@ ``int64 cast`` @>

[<Test>]
let ``Time int64 cast`` () =
    timeFunction <@ ``int64 cast`` @>


[<ReflectedDefinition; TimeAllowance 1.1>]
let ``id function`` () =
    let rand = Random 3141592
    let mutable total = 0
    for i = 0 to 1000 do
        total <- id (rand.Next ())
    total

[<Test>]
let ``Test id function`` () =
    testFunction <@ ``id function`` @>

[<Test>]
let ``Time id function`` () =
    timeFunction <@ ``id function`` @>


[<ReflectedDefinition; TimeAllowance 1.1>]
let ``operator |>`` () =
    let rand = Random 3141592
    let mutable total = 0
    for i = 0 to 1000 do
        total <- (rand.Next ()) |> id
    total

[<Test>]
let ``Test operator |>`` () =
    testFunction <@ ``operator |>`` @>

[<Test>]
let ``Time operator |>`` () =
    timeFunction <@ ``operator |>`` @>


[<ReflectedDefinition; TimeAllowance 1.1>]
let ``operator <|`` () =
    let rand = Random 3141592
    let mutable total = 0
    for i = 0 to 1000 do
        total <- id <| (rand.Next ())
    total

[<Test>]
let ``Test operator <|`` () =
    testFunction <@ ``operator <|`` @>

[<Test>]
let ``Time operator <|`` () =
    timeFunction <@ ``operator <|`` @>

(*
[<ReflectedDefinition>]
let ``[]()`` () =

[<Test>]
let `` `` () =
    testFunction <@ `` `` @>

[<Test>]
let `` `` () =
    timeFunction <@ `` `` @>
*)