module FSharp.Quotations.Evaluator.Performance

open System
open System.Diagnostics
open System.Reflection
open System.Linq.Expressions
open Xunit
open FSharp.Quotations
open FSharp.Quotations.Evaluator.Unittests

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

//// we probably have a lot of ceremony here for no particular reason, but it shouldn't hurt
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

    Assert.True(viaLinqMs <= allowedTime, 
        sprintf "Took too long! linq=%g compiled=%g multiples of allowed time=%g" 
            viaLinqMs directMs (viaLinqMs / allowedTime))

    Assert.True (viaLinqMs >= allowedTime * 0.75, 
        sprintf "Too fast; decrease the multiplier! linq=%g compiled=%g allowed multiples=%g-%g actual multiples=%g"
            viaLinqMs directMs (timeAllowanceMultiplier * 0.75) timeAllowanceMultiplier (viaLinqMs / directMs))

#if INCLUDE_TIMING_TESTS
type TimeFactAttribute = FactAttribute
#else
type TimeFactAttribute() = inherit FactAttribute(Skip = "Ignoring timing tests. Set INCLUDE_TIMING_TESTS")
#endif

[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.25>]
let ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    let ToggleNth n (lst:bool array) =                  // Toggle every n'th door
        [(n-1) .. n .. 99]                              // For each appropriate door
        |> Seq.iter (fun i -> lst.[i] <- not lst.[i])   // toggle it
    let doors = Array.create 100 false                  // Initialize all doors to closed
    Seq.iter (fun n -> ToggleNth n doors) [1..100]      // toggle the appropriate doors for each pass
    doors 

[<Fact>]
let ``Test [answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    testFunction <@ ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` @>

[<TimeFact>]
let ``Time [answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    timeFunction <@ ``[answerDoors](http://rosettacode.org/wiki/100_doors#F.23)`` @>


[<ReflectedDefinition; TimeAllowance 1.25>]
let ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    let PerfectSquare n =
        let sqrt = int(Math.Sqrt(float n))
        n = sqrt * sqrt
    [| for i in 1..100 do yield PerfectSquare i |]    

[<Fact>]
let ``Test [answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    testFunction <@ ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` @>

[<TimeFact>]
let ``Time [answer2](http://rosettacode.org/wiki/100_doors#F.23)`` () =
    timeFunction <@ ``[answer2](http://rosettacode.org/wiki/100_doors#F.23)`` @>


[<ReflectedDefinition; TimeAllowance 1.5>]
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

[<Fact>]
let ``Test [Euler_method](http://rosettacode.org/wiki/Euler_method#F.23)`` () =
    testFunction <@ ``[Euler_method](http://rosettacode.org/wiki/Euler_method#F.23)`` @>

[<TimeFact>]
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

[<Fact>]
let ``Test int Operators +-/*%`` () =
    testFunction <@ ``int Operators +-/*%`` @>

[<TimeFact>]
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

[<Fact>]
let ``Test int64 Operators +-/*%`` () =
    testFunction <@ ``int64 Operators +-/*%`` @>

[<TimeFact>]
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

[<Fact>]
let ``Test float Operators +-/*%`` () =
    testFunction <@ ``float Operators +-/*%`` @>

[<TimeFact>]
let ``Time float Operators +-/*%`` () =
    timeFunction <@ ``float Operators +-/*%`` @>

(*
fails to run compiled version; I think because of FSharp.Core.ExtraTopLevelOperators.ToSingle

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

[<Fact>]
let ``Test single Operators +-/*%`` () =
    testFunction <@ ``single Operators +-/*%`` @>

[<Fact>]
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

[<Fact>]
let ``Test int Operators <>=`` () =
    testFunction <@ ``int Operators <>=`` @>

[<TimeFact>]
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

[<Fact>]
let ``Test int64 Operators <>=`` () =
    testFunction <@ ``int64 Operators <>=`` @>

[<TimeFact>]
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

[<Fact>]
let ``Test float Operators <>=`` () =
    testFunction <@ ``float Operators <>=`` @>

[<TimeFact>]
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

[<Fact>]
let ``Test single Operators <>=`` () =
    testFunction <@ ``single Operators <>=`` @>

[<TimeFact>]
let ``Time single Operators <>=`` () =
    timeFunction <@ ``single Operators <>=`` @>


[<ReflectedDefinition; TimeAllowance 1.2>]
let ``float cast`` () =
    let rand = Random 3141592
    let mutable total = 0.0
    for i=0 to 1000 do
        total <- total + (float (rand.Next ()))
    total

[<Fact>]
let ``Test float cast`` () =
    testFunction <@ ``float cast`` @>

[<TimeFact>]
let ``Time float cast`` () =
    timeFunction <@ ``float cast`` @>


[<ReflectedDefinition; TimeAllowance 1.2>]
let ``int64 cast`` () =
    let rand = Random 3141592
    let mutable total = 0L
    for i=0 to 1000 do
        total <- total + (int64 (rand.Next ()))
    total

[<Fact>]
let ``Test int64 cast`` () =
    testFunction <@ ``int64 cast`` @>

[<TimeFact>]
let ``Time int64 cast`` () =
    timeFunction <@ ``int64 cast`` @>


[<ReflectedDefinition; TimeAllowance 1.1>]
let ``id function`` () =
    let rand = Random 3141592
    let mutable total = 0
    for i = 0 to 1000 do
        total <- id (rand.Next ())
    total

[<Fact>]
let ``Test id function`` () =
    testFunction <@ ``id function`` @>

[<TimeFact>]
let ``Time id function`` () =
    timeFunction <@ ``id function`` @>


[<ReflectedDefinition; TimeAllowance 1.2>]
let ``operator |>`` () =
    let rand = Random 3141592
    let mutable total = 0
    for i = 0 to 1000 do
        total <- (rand.Next ()) |> id
    total

[<Fact>]
let ``Test operator |>`` () =
    testFunction <@ ``operator |>`` @>

[<TimeFact>]
let ``Time operator |>`` () =
    timeFunction <@ ``operator |>`` @>


[<ReflectedDefinition; TimeAllowance 1.2>]
let ``operator <|`` () =
    let rand = Random 3141592
    let mutable total = 0
    for i = 0 to 1000 do
        total <- id <| (rand.Next ())
    total

[<Fact>]
let ``Test operator <|`` () =
    testFunction <@ ``operator <|`` @>

[<TimeFact>]
let ``Time operator <|`` () =
    timeFunction <@ ``operator <|`` @>

[<ReflectedDefinition; TestIterations 1000; TimeAllowance 1.1>]
let ``operator .. ..`` () =
    let mutable n = 0.0
    for i in { 1 .. 3 .. 100000 } do
        n <- float i
    n

[<Fact>]
let ``Test operator .. ..`` () =
    testFunction <@ ``operator .. ..`` @>

[<TimeFact>]
let ``Time operator .. ..`` () =
    timeFunction <@ ``operator .. ..`` @>


[<ReflectedDefinition; TestIterations 100; TimeAllowance 1.5>]
let ``recursion is even or odd`` () =
    let rec isEven n =
        if n = 0 then true
        else isOdd (n-1)
    and isOdd n =
        if n = 0 then false
        else isEven (n-1)
    
    let r = Random 3141592
    Seq.init 100 (fun _ -> if isEven (r.Next 10000) then 1 else -1)
    |> Seq.sum

[<Fact>]
let ``Test recursion is even or odd`` () =
    testFunction <@ ``recursion is even or odd`` @>

[<TimeFact>]
let ``Time recursion is even or odd`` () =
    timeFunction <@ ``recursion is even or odd`` @>

[<ReflectedDefinition; TestIterations 100; TimeAllowance 15.0>]
let ``recursion fibonacci`` () =
    let rec fib n =
        if n <= 2 then 1
        else fib (n-1) + fib (n-2)
    
    let r = Random 3141592
    Seq.init 10 (fun _ -> float <| fib (r.Next 30))
    |> Seq.sum

[<Fact>]
let ``Test recursion fibonacci`` () =
    testFunction <@ ``recursion fibonacci`` @>

[<TimeFact>]
let ``Time recursion fibonacci`` () =
    timeFunction <@ ``recursion fibonacci`` @>

type Partner = {
    Name : string
    Partner : Partner
}

[<ReflectedDefinition; TimeAllowance 15.0>]
let ``recursion type creation`` () =
    let rec penny  = { 
        Name = "Penny"
        Partner = paul }
    and paul = {
        Name = "Paul"
        Partner = penny }

    penny.Name + "&" + paul.Name = paul.Partner.Name + "&" + penny.Partner.Name

[<Fact(Skip = "Not currently supported")>]
let ``Test recursion type creation`` () =
    testFunction <@ ``recursion type creation`` @>

[<TimeFact(Skip = "Not currently supported")>]
let ``Time recursion type creation`` () =
    timeFunction <@ ``recursion type creation`` @>


[<ReflectedDefinition; TimeAllowance 15.0>]
let ``recursion tail`` () =
    let rec count acc n = 
        if n = 0 then acc
        else count (acc+1) (n-1)

    count 0 100000

[<Fact(Skip = "Not currently supported - blows up")>]
let ``Test recursion tail`` () =
    testFunction <@ ``recursion tail`` @>

[<TimeFact(Skip = "Not currently supported - blows up")>]
let ``Time recursion tail`` () =
    timeFunction <@ ``recursion tail`` @>

[<ReflectedDefinition; TimeAllowance 9.0>]
let ``many captures and parameters 1`` () =
    let ff a b c d e f =
        let gg a' b' c' d' e' f' =
            a' * a + b' * b - c' * c + d' * d - e' * e + f' * f
        gg
    let r = Random 42
    let x () = r.Next()
    let mutable total = 0
    let fff = ff (x()) (x()) (x()) (x()) (x()) (x()) 
    for i=0 to 10 do
        total <- total + fff (x()) (x()) (x()) (x()) (x()) (x())
    total

[<Fact>]
let ``Test many captures and parameters 1`` () =
    testFunction <@ ``many captures and parameters 1`` @>

[<TimeFact>]
let ``Time many captures and parameters 1`` () =
    timeFunction <@ ``many captures and parameters 1`` @>

[<ReflectedDefinition; TimeAllowance 3.5>]
let ``many captures and parameters 2`` () =
    let ff a b c d e f g h i j k l m n o =
        let gg a' = a' * a + a' * b - a' * c + a' * d - a' * e + a' * f - a' * g + a' * h - a' * i + a' * j - a' * k + a' * l - a' * m + a' * n - a' * o
        gg
    let r = Random 42
    let x () = r.Next()
    let mutable total = 0
    let fff = ff (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x())
    for i=0 to 10 do
        total <- total + fff (x())
    total

[<Fact>]
let ``Test many captures and parameters 2`` () =
    testFunction <@ ``many captures and parameters 2`` @>

[<TimeFact>]
let ``Time many captures and parameters 2`` () =
    timeFunction <@ ``many captures and parameters 2`` @>

[<ReflectedDefinition; TimeAllowance 13.0>]
let ``many captures and parameters 3`` () =
    let ff a =
        let gg a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p' q' r' s' =
            a' * a + b' * a - c' * a + d' * a - e' * a + f' * a - g' * a + h' * a - i' * a + j' * a - k' * a + l' * a - m' * a + n' * a - o' * a + p' * a - q' * a + r' * a - s' * a
        gg
    let r = Random 42
    let x () = r.Next()
    let mutable total = 0
    let fff = ff (x())
    for i=0 to 10 do
        total <- total + fff (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x())
    total

[<Fact>]
let ``Test many captures and parameters 3`` () =
    testFunction <@ ``many captures and parameters 3`` @>

[<TimeFact>]
let ``Time many captures and parameters 3`` () =
    timeFunction <@ ``many captures and parameters 3`` @>

[<ReflectedDefinition; TimeAllowance 9.0>]
let ``many captures and parameters 4`` () =
    let ff a b c d e f g h i j k l m n o =
        let gg a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' =
            a' * a + b' * b - c' * c + d' * d - e' * e + f' * f - g' * g + h' * h - i' * i + j' * j - k' * k + l' * l - m' * m + n' * n - o' * o
        gg
    let r = Random 42
    let x () = r.Next()
    let mutable total = 0
    let fff = ff (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x()) (x())
    for i=0 to 10 do
        total <- total + fff (x()) (x()) (x()) (x())
    total

[<Fact>]
let ``Test many captures and parameters 4`` () =
    testFunction <@ ``many captures and parameters 4`` @>

[<TimeFact>]
let ``Time many captures and parameters 4`` () =
    timeFunction <@ ``many captures and parameters 4`` @>


(*
[<ReflectedDefinition>]
let ``[]()`` () =

[<Fact>]
let `` `` () =
    testFunction <@ `` `` @>

[<Fact>]
let `` `` () =
    timeFunction <@ `` `` @>
*)