module FSharp.Quotations.Evaluator.Tools

open System.Linq.Expressions
open System

let createTuple<'a> types =
    let tuple = typedefof<'a>.MakeGenericType types
    let ``constructor`` = tuple.GetConstructor types
    let callConstructor (x:seq<Expression>) = 
        Expression.New (``constructor``, x) :> Expression
    tuple, callConstructor

let rec createGenericTupleType types =
    match types with
    | [||]                      -> typeof<Unit>, (fun _ -> Expression.Constant(null, typeof<unit>) :> Expression)
    | [|t1|]                    -> createTuple<Tuple<_>>             types
    | [|t1;t2|]                 -> createTuple<Tuple<_,_>>           types
    | [|t1;t2;t3|]              -> createTuple<Tuple<_,_,_>>         types
    | [|t1;t2;t3;t4|]           -> createTuple<Tuple<_,_,_,_>>       types
    | [|t1;t2;t3;t4;t5|]        -> createTuple<Tuple<_,_,_,_,_>>     types
    | [|t1;t2;t3;t4;t5;t6|]     -> createTuple<Tuple<_,_,_,_,_,_>>   types
    | [|t1;t2;t3;t4;t5;t6;t7|]  -> createTuple<Tuple<_,_,_,_,_,_,_>> types
    | _ ->
        let slice = types.[7..]
        let innerTuple, create = createGenericTupleType slice
        let sliceTypes = [| yield! types.[0..6]; yield innerTuple |]
        let tuple = typedefof<Tuple<_,_,_,_,_,_,_,_>>.MakeGenericType sliceTypes
        let ``constructor`` = tuple.GetConstructor sliceTypes
        let callConstructor (x:seq<Expression>) =
            let args = [|
                yield! Seq.take 7 x 
                yield create (Seq.skip 7 x) |]
            Expression.New(``constructor``, args) :> Expression
        tuple, callConstructor


