module Tools

open FSharp.Quotations.Evaluator.Tools
open NUnit.Framework
open System
open System.Linq.Expressions

[<AutoOpen>]
module private Local =
    type T1  = class end
    type T2  = class end
    type T3  = class end
    type T4  = class end
    type T5  = class end
    type T6  = class end
    type T7  = class end
    type T8  = class end
    type T9  = class end
    type T10 = class end
    type T11 = class end
    type T12 = class end
    type T13 = class end
    type T14 = class end
    type T15 = class end
    type T16 = class end
    type T17 = class end
    type T18 = class end
    type T19 = class end
    type T20 = class end

    let inline constant value = Expression.Constant value

[<Test>]
let ``Test createGenericTupleType types`` () =
    let tInt, _ = createGenericTupleType [|typeof<T1>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>;typeof<T14>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13,T14>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>;typeof<T14>;typeof<T15>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13,T14,Tuple<T15>>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>;typeof<T14>;typeof<T15>;typeof<T16>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13,T14,Tuple<T15,T16>>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>;typeof<T14>;typeof<T15>;typeof<T16>;typeof<T17>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13,T14,Tuple<T15,T16,T17>>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>;typeof<T14>;typeof<T15>;typeof<T16>;typeof<T17>;typeof<T18>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13,T14,Tuple<T15,T16,T17,T18>>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>;typeof<T14>;typeof<T15>;typeof<T16>;typeof<T17>;typeof<T18>;typeof<T19>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13,T14,Tuple<T15,T16,T17,T18,T19>>>>)
    let tInt, _ = createGenericTupleType [|typeof<T1>;typeof<T2>;typeof<T3>;typeof<T4>;typeof<T5>;typeof<T6>;typeof<T7>;typeof<T8>;typeof<T9>;typeof<T10>;typeof<T11>;typeof<T12>;typeof<T13>;typeof<T14>;typeof<T15>;typeof<T16>;typeof<T17>;typeof<T18>;typeof<T19>;typeof<T20>|]
    Assert.AreEqual(tInt, typeof<Tuple<T1,T2,T3,T4,T5,T6,T7,Tuple<T8,T9,T10,T11,T12,T13,T14,Tuple<T15,T16,T17,T18,T19,T20>>>>)
    ()

let construct<'a> getConstructor args =
    let ``constructor`` = getConstructor args
    let creator = Expression.Lambda<Func<'a>>(``constructor``).Compile()
    creator.Invoke ()

[<Test>]
let ``Test createGenericTupleType construction`` () =
    let _, getConstructor = createGenericTupleType [|typeof<int>|]
    let result = construct<Tuple<int>> getConstructor [| constant 1 |]
    Assert.AreEqual(Tuple.Create 1, result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int>> getConstructor [| constant 1; constant 2 |]
    Assert.AreEqual(Tuple.Create (1,2), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int>> getConstructor [| constant 1; constant 2; constant 3 |]
    Assert.AreEqual(Tuple.Create (1,2,3), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int>> getConstructor [| constant 1; constant 2; constant 3; constant 4 |]
    Assert.AreEqual(Tuple.Create (1,2,3,4), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5 |]
    Assert.AreEqual(Tuple.Create (1,2,3,4,5), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6 |]
    Assert.AreEqual(Tuple.Create (1,2,3,4,5,6), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7 |]
    Assert.AreEqual(Tuple.Create (1,2,3,4,5,6,7), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8 |]
    Assert.AreEqual(Tuple.Create (1,2,3,4,5,6,7,8), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int,int>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13; constant 14 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13,14), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int,int,Tuple<int>>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13; constant 14 ; constant 15 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int,int,Tuple<int,int>>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13; constant 14 ; constant 15 ; constant 16 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int,int,Tuple<int,int,int>>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13; constant 14 ; constant 15 ; constant 16 ; constant 17 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int>>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13; constant 14 ; constant 15 ; constant 16 ; constant 17; constant 18 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int>>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13; constant 14 ; constant 15 ; constant 16 ; constant 17; constant 18 ; constant 19 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), result)

    let _, getConstructor = createGenericTupleType [|typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>;typeof<int>|]
    let result = construct<Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int,int,Tuple<int,int,int,int,int,int>>>> getConstructor [| constant 1; constant 2; constant 3; constant 4; constant 5; constant 6; constant 7; constant 8; constant 9; constant 10 ; constant 11; constant 12; constant 13; constant 14 ; constant 15 ; constant 16 ; constant 17; constant 18 ; constant 19 ; constant 20 |]
    Assert.AreEqual((1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), result)
