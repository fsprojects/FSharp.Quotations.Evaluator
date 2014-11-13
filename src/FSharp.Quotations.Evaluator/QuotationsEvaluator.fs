// Copyright (c) Microsoft Corporation 2005-2008.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 
//


namespace FSharp.Quotations.Evaluator

open System
open System.Linq
open System.Collections.Generic
open System.Linq.Expressions
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns



module ExtraHashCompare =
    let GenericNotEqualIntrinsic<'T> (x:'T) (y:'T) : bool = not (Microsoft.FSharp.Core.LanguagePrimitives.HashCompare.GenericEqualityIntrinsic<'T> x y)


module QuotationEvaluationTypes = 
    
    type This = 
        static member Assembly = typeof<This>.Assembly

    let hashCompareType = typeof<list<_>>.Assembly.GetType("Microsoft.FSharp.Core.LanguagePrimitives+HashCompare")
    let extraHashCompareType = This.Assembly.GetType("FSharp.Quotations.Evaluator.ExtraHashCompare")
    let genericEqualityIntrinsic = "GenericEqualityIntrinsic" |> hashCompareType.GetMethod
    let genericNotEqualIntrinsic = "GenericNotEqualIntrinsic" |> extraHashCompareType.GetMethod
    let genericLessThanIntrinsic = "GenericLessThanIntrinsic" |> hashCompareType.GetMethod
    let genericGreaterThanIntrinsic = "GenericGreaterThanIntrinsic" |> hashCompareType.GetMethod
    let genericGreaterOrEqualIntrinsic = "GenericGreaterOrEqualIntrinsic" |> hashCompareType.GetMethod
    let genericLessOrEqualIntrinsic = "GenericLessOrEqualIntrinsic" |> hashCompareType.GetMethod


    type ConvEnv = 
        {   eraseEquality : bool;
            varEnv : Map<Var,Expression>
        }
    let asExpr x = (x :> Expression)

    let bindingFlags = BindingFlags.Public ||| BindingFlags.NonPublic
    let instanceBindingFlags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly
    let isNamedType(typ:Type) = not (typ.IsArray || typ.IsByRef || typ.IsPointer)
    let equivHeadTypes (ty1:Type) (ty2:Type) = 
        isNamedType(ty1) &&
        if ty1.IsGenericType then 
          ty2.IsGenericType && (ty1.GetGenericTypeDefinition()).Equals(ty2.GetGenericTypeDefinition())
        else 
          ty1.Equals(ty2)

    let isFunctionType typ = equivHeadTypes typ (typeof<(int -> int)>)
    let getFunctionType typ = 
        if not (isFunctionType typ) then invalidArg "typ" "cannot convert recursion except for function types"
        let tyargs = typ.GetGenericArguments()
        tyargs.[0], tyargs.[1]
    

    let ArrayAssignHelper (arr : 'T[]) (idx:int) (elem:'T) : 'unt = 
        arr.[idx] <- elem;
        unbox (box ())


    let TryFinallyHelper e h = 
        try e() 
        finally h()

    let TryWithHelper e filter handler = 
        try e() 
        with e when (filter e <> 0) -> handler e

    let ArrayAssignMethod = match <@@ ArrayAssignHelper @@> with Lambdas(_,Call(_,minfo,_)) -> minfo | _ -> failwith "couldn't find minfo"
    let TryFinallyMethod = match <@@ TryFinallyHelper @@> with Lambdas(_,Call(_,minfo,_)) -> minfo | _ -> failwith "couldn't find minfo"
    let TryWithMethod = match <@@ TryWithHelper @@> with Lambdas(_,Call(_,minfo,_)) -> minfo | _ -> failwith "couldn't find minfo"

    module HelperTypes = 
        type ActionHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 -> unit
        type ActionHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17, 'T18> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 * 'T18 -> unit
        type ActionHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17, 'T18, 'T19> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 * 'T18 * 'T19 -> unit
        type ActionHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17, 'T18, 'T19, 'T20> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 * 'T18 * 'T19 * 'T20 -> unit

        type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17, 'T18> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 -> 'T18 
        type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17, 'T18, 'T19> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 * 'T18 -> 'T19 
        type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17, 'T18, 'T19, 'T20> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 * 'T18 * 'T19 -> 'T20 
        type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10, 'T11, 'T12, 'T13, 'T14, 'T15, 'T16, 'T17, 'T18, 'T19, 'T20, 'T21> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 * 'T9 * 'T10 * 'T11 * 'T12 * 'T13 * 'T14 * 'T15 * 'T16 * 'T17 * 'T18 * 'T19 * 'T20 -> 'T21 

    open HelperTypes
    
    let GetActionType (args:Type[])  = 
        if args.Length <= 16 then 
            Expression.GetActionType args
        else
            match args.Length with 
            | 17 -> typedefof<ActionHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | 18 -> typedefof<ActionHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | 19 -> typedefof<ActionHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | 20 -> typedefof<ActionHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | _ -> raise <| new NotSupportedException("Quotation expressions with statements or closures containing more then 20 free variables may not be translated in this release of the F# PowerPack. This is due to limitations in the variable binding expression forms available in LINQ expression trees")

    let GetFuncType (args:Type[])  = 
        if args.Length <= 17 then 
            Expression.GetFuncType args
        else
            match args.Length with 
            | 18 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | 19 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | 20 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | 21 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
            | _ -> raise <| new NotSupportedException("Quotation expressions with statements or closures containing more then 20 free variables may not be translated in this release of the F# PowerPack. This is due to limitations in the variable binding expression forms available in LINQ expression trees")
            

    let LetRec1Helper (F1:System.Func<_,_,_>) (B:System.Func<_,_>) = 
        let fhole = ref (Unchecked.defaultof<_>)
        let f = new System.Func<_,_>(fun x -> F1.Invoke(fhole.Value,x))
        fhole := f
        B.Invoke f

    let LetRec2Helper (F1:System.Func<_,_,_,_>) (F2:System.Func<_,_,_,_>) (B:System.Func<_,_,_>) = 
        let f1hole = ref (Unchecked.defaultof<_>)
        let f2hole = ref (Unchecked.defaultof<_>)
        let f1 = new System.Func<_,_>(fun x -> F1.Invoke(f1hole.Value,f2hole.Value,x))
        let f2 = new System.Func<_,_>(fun x -> F2.Invoke(f1hole.Value,f2hole.Value,x))
        f1hole := f1
        f2hole := f2
        B.Invoke(f1,f2)

    let LetRec3Helper (F1:System.Func<_,_,_,_,_>) (F2:System.Func<_,_,_,_,_>) (F3:System.Func<_,_,_,_,_>) (B:System.Func<_,_,_,_>) = 
        let f1hole = ref (Unchecked.defaultof<_>)
        let f2hole = ref (Unchecked.defaultof<_>)
        let f3hole = ref (Unchecked.defaultof<_>)
        let f1 = new System.Func<_,_>(fun x -> F1.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,x))
        let f2 = new System.Func<_,_>(fun x -> F2.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,x))
        let f3 = new System.Func<_,_>(fun x -> F3.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,x))
        f1hole := f1
        f2hole := f2
        f3hole := f3
        B.Invoke(f1,f2,f3)

    let LetRec4Helper 
           (F1:System.Func<_,_,_,_,_,_>) 
           (F2:System.Func<_,_,_,_,_,_>) 
           (F3:System.Func<_,_,_,_,_,_>) 
           (F4:System.Func<_,_,_,_,_,_>) 
           (B:System.Func<_,_,_,_,_>) = 
        let f1hole = ref (Unchecked.defaultof<_>)
        let f2hole = ref (Unchecked.defaultof<_>)
        let f3hole = ref (Unchecked.defaultof<_>)
        let f4hole = ref (Unchecked.defaultof<_>)
        let f1 = new System.Func<_,_>(fun x -> F1.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,x))
        let f2 = new System.Func<_,_>(fun x -> F2.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,x))
        let f3 = new System.Func<_,_>(fun x -> F3.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,x))
        let f4 = new System.Func<_,_>(fun x -> F4.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,x))
        f1hole := f1
        f2hole := f2
        f3hole := f3
        f4hole := f4
        B.Invoke(f1,f2,f3,f4)


    let LetRec5Helper 
           (F1:System.Func<_,_,_,_,_,_,_>) 
           (F2:System.Func<_,_,_,_,_,_,_>) 
           (F3:System.Func<_,_,_,_,_,_,_>) 
           (F4:System.Func<_,_,_,_,_,_,_>) 
           (F5:System.Func<_,_,_,_,_,_,_>) 
           (B:System.Func<_,_,_,_,_,_>) = 
        let f1hole = ref (Unchecked.defaultof<_>)
        let f2hole = ref (Unchecked.defaultof<_>)
        let f3hole = ref (Unchecked.defaultof<_>)
        let f4hole = ref (Unchecked.defaultof<_>)
        let f5hole = ref (Unchecked.defaultof<_>)
        let f1 = new System.Func<_,_>(fun x -> F1.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,x))
        let f2 = new System.Func<_,_>(fun x -> F2.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,x))
        let f3 = new System.Func<_,_>(fun x -> F3.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,x))
        let f4 = new System.Func<_,_>(fun x -> F4.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,x))
        let f5 = new System.Func<_,_>(fun x -> F5.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,x))
        f1hole := f1
        f2hole := f2
        f3hole := f3
        f4hole := f4
        f5hole := f5
        B.Invoke(f1,f2,f3,f4,f5)

    let LetRec6Helper 
           (F1:System.Func<_,_,_,_,_,_,_,_>) 
           (F2:System.Func<_,_,_,_,_,_,_,_>) 
           (F3:System.Func<_,_,_,_,_,_,_,_>) 
           (F4:System.Func<_,_,_,_,_,_,_,_>) 
           (F5:System.Func<_,_,_,_,_,_,_,_>) 
           (F6:System.Func<_,_,_,_,_,_,_,_>) 
           (B:System.Func<_,_,_,_,_,_,_>) = 
        let f1hole = ref (Unchecked.defaultof<_>)
        let f2hole = ref (Unchecked.defaultof<_>)
        let f3hole = ref (Unchecked.defaultof<_>)
        let f4hole = ref (Unchecked.defaultof<_>)
        let f5hole = ref (Unchecked.defaultof<_>)
        let f6hole = ref (Unchecked.defaultof<_>)
        let f1 = new System.Func<_,_>(fun x -> F1.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,x))
        let f2 = new System.Func<_,_>(fun x -> F2.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,x))
        let f3 = new System.Func<_,_>(fun x -> F3.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,x))
        let f4 = new System.Func<_,_>(fun x -> F4.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,x))
        let f5 = new System.Func<_,_>(fun x -> F5.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,x))
        let f6 = new System.Func<_,_>(fun x -> F6.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,x))
        f1hole := f1
        f2hole := f2
        f3hole := f3
        f4hole := f4
        f5hole := f5
        f6hole := f6
        B.Invoke(f1,f2,f3,f4,f5,f6)


    let LetRec7Helper 
           (F1:System.Func<_,_,_,_,_,_,_,_,_>) 
           (F2:System.Func<_,_,_,_,_,_,_,_,_>) 
           (F3:System.Func<_,_,_,_,_,_,_,_,_>) 
           (F4:System.Func<_,_,_,_,_,_,_,_,_>) 
           (F5:System.Func<_,_,_,_,_,_,_,_,_>) 
           (F6:System.Func<_,_,_,_,_,_,_,_,_>) 
           (F7:System.Func<_,_,_,_,_,_,_,_,_>) 
           (B:System.Func<_,_,_,_,_,_,_,_>) = 
        let f1hole = ref (Unchecked.defaultof<_>)
        let f2hole = ref (Unchecked.defaultof<_>)
        let f3hole = ref (Unchecked.defaultof<_>)
        let f4hole = ref (Unchecked.defaultof<_>)
        let f5hole = ref (Unchecked.defaultof<_>)
        let f6hole = ref (Unchecked.defaultof<_>)
        let f7hole = ref (Unchecked.defaultof<_>)
        let f1 = new System.Func<_,_>(fun x -> F1.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,x))
        let f2 = new System.Func<_,_>(fun x -> F2.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,x))
        let f3 = new System.Func<_,_>(fun x -> F3.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,x))
        let f4 = new System.Func<_,_>(fun x -> F4.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,x))
        let f5 = new System.Func<_,_>(fun x -> F5.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,x))
        let f6 = new System.Func<_,_>(fun x -> F6.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,x))
        let f7 = new System.Func<_,_>(fun x -> F7.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,x))
        f1hole := f1
        f2hole := f2
        f3hole := f3
        f4hole := f4
        f5hole := f5
        f6hole := f6
        f7hole := f7
        B.Invoke(f1,f2,f3,f4,f5,f6,f7)


    let LetRec8Helper 
           (F1:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (F2:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (F3:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (F4:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (F5:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (F6:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (F7:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (F8:System.Func<_,_,_,_,_,_,_,_,_,_>) 
           (B:System.Func<_,_,_,_,_,_,_,_,_>) = 
        let f1hole = ref (Unchecked.defaultof<_>)
        let f2hole = ref (Unchecked.defaultof<_>)
        let f3hole = ref (Unchecked.defaultof<_>)
        let f4hole = ref (Unchecked.defaultof<_>)
        let f5hole = ref (Unchecked.defaultof<_>)
        let f6hole = ref (Unchecked.defaultof<_>)
        let f7hole = ref (Unchecked.defaultof<_>)
        let f8hole = ref (Unchecked.defaultof<_>)
        let f1 = new System.Func<_,_>(fun x -> F1.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        let f2 = new System.Func<_,_>(fun x -> F2.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        let f3 = new System.Func<_,_>(fun x -> F3.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        let f4 = new System.Func<_,_>(fun x -> F4.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        let f5 = new System.Func<_,_>(fun x -> F5.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        let f6 = new System.Func<_,_>(fun x -> F6.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        let f7 = new System.Func<_,_>(fun x -> F7.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        let f8 = new System.Func<_,_>(fun x -> F8.Invoke(f1hole.Value,f2hole.Value,f3hole.Value,f4hole.Value,f5hole.Value,f6hole.Value,f7hole.Value,f8hole.Value,x))
        f1hole := f1
        f2hole := f2
        f3hole := f3
        f4hole := f4
        f5hole := f5
        f6hole := f6
        f7hole := f7
        f8hole := f8
        B.Invoke(f1,f2,f3,f4,f5,f6,f7,f8)

    type FuncFSharp<'state,'a> (f:Func<'state,'a>, state:'state) =
        inherit FSharpFunc<unit, 'a>()
        override __.Invoke _ = f.Invoke state

    type FuncFSharp<'state,'a,'b> (f:Func<'state,'a,'b>, state:'state) =
        inherit FSharpFunc<'a,'b>()
        override __.Invoke a = f.Invoke (state,a)

    type FuncFSharp<'state,'a,'b,'c> (f:Func<'state,'a,'b,'c>, state:'state) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c>()
        override __.Invoke (a,b) = f.Invoke (state,a,b)
        override this.Invoke a = fun b -> this.Invoke (a,b)

    type FuncFSharp<'state,'a,'b,'c,'d> (f:Func<'state,'a,'b,'c,'d>, state:'state) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d>()
        override __.Invoke (a,b,c) = f.Invoke (state,a,b,c)
        override this.Invoke a = fun b c -> this.Invoke (a,b,c)

    type FuncFSharp<'state,'a,'b,'c,'d,'e> (f:Func<'state,'a,'b,'c,'d,'e>, state:'state) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e>()
        override __.Invoke (a,b,c,d) = f.Invoke (state,a,b,c,d)
        override this.Invoke a = fun b c d -> this.Invoke (a,b,c,d)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f> (f:Func<'state,'a,'b,'c,'d,'e,'f>, state:'state) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f>()
        override __.Invoke (a,b,c,d,e) = f.Invoke (state,a,b,c,d,e)
        override this.Invoke a = fun b c d e -> this.Invoke (a,b,c,d,e)

    let IsVoidType (ty:System.Type)  = (ty = typeof<System.Void>)

    let LinqExpressionHelper (x:'T) : Expression<'T> = failwith ""
    
    let MakeFakeExpression (x:Expr) = 
        let minfo = match <@@ LinqExpressionHelper @@> with Lambda(_,Call(_,minfo,_)) -> minfo | _ -> failwith "couldn't find method info"
        Expr.Call(minfo.GetGenericMethodDefinition().MakeGenericMethod [| x.Type |], [ x ])

    let showAll = BindingFlags.Public ||| BindingFlags.NonPublic 

    let wrapVoid (e:#Expression) =
        if e.Type <> typeof<System.Void> then e |> asExpr
        else 
            Expression.Block(
                e,
                Expression.Constant(null, typeof<Unit>)) |> asExpr

    let (|Λ|_|) (``method``:MethodInfo) = function
    | Patterns.Call (o, methodInfo, args) when methodInfo.Name = ``method``.Name ->
        if methodInfo.IsGenericMethod then
            let generic = methodInfo.GetGenericMethodDefinition() 
            if ``method`` = generic then
                let genericArgs = methodInfo.GetGenericArguments ()
                Some (o, genericArgs, args)
            else
                None
        elif ``method`` = methodInfo then
            Some (o, [||], args)
        else None
    | _ -> None

    let rec getMethodInfo = function
    | Patterns.Call(_,``method``,_) -> ``method``
    | Patterns.Lambda(_,body) -> getMethodInfo body
    | _ -> failwith "Unexpected Form"

    let getGenericMethodInfo functionExpression =
        let methodInfo = getMethodInfo functionExpression
        if methodInfo.IsGenericMethod then
            methodInfo.GetGenericMethodDefinition ()
        else
            methodInfo

    let ``-> not`` = getGenericMethodInfo <@ not @>

    let ``-> generic=`` = getGenericMethodInfo <@ LanguagePrimitives.GenericEquality @>
    let ``-> =``  = getGenericMethodInfo <@ ( =  ) @>
    let ``-> >``  = getGenericMethodInfo <@ ( >  ) @>
    let ``-> >=`` = getGenericMethodInfo <@ ( >= ) @>
    let ``-> <``  = getGenericMethodInfo <@ ( <  ) @>
    let ``-> <=`` = getGenericMethodInfo <@ ( <= ) @>
    let ``-> <>`` = getGenericMethodInfo <@ ( <> ) @>

    let ``-> ~-`` = getGenericMethodInfo <@ ( ~-) : int -> int @>
    let ``-> +`` = getGenericMethodInfo <@ (+) @>
    let ``-> /`` = getGenericMethodInfo <@ (/) @>
    let ``-> -`` = getGenericMethodInfo <@ (-) @>
    let ``-> *`` = getGenericMethodInfo <@ (*) @>
    let ``-> %`` = getGenericMethodInfo <@ (%) @>

    let ``-> <<<`` = getGenericMethodInfo <@ (<<<) @>
    let ``-> >>>`` = getGenericMethodInfo <@ (>>>) @>
    let ``-> &&&`` = getGenericMethodInfo <@ (&&&) @>
    let ``-> |||`` = getGenericMethodInfo <@ (|||) @>
    let ``-> ^^^`` = getGenericMethodInfo <@ (^^^) @>
    let ``-> ~~~`` = getGenericMethodInfo <@ (~~~) @>

    let ``-> checked~-`` = getGenericMethodInfo <@ Checked.(~-) : int -> int @>
    let ``-> checked+``  = getGenericMethodInfo <@ Checked.(+) @>
    let ``-> checked-``  = getGenericMethodInfo <@ Checked.(-) @>
    let ``-> checked*``  = getGenericMethodInfo <@ Checked.(*) @>

    let ``-> char``    = getGenericMethodInfo <@ char @>
    let ``-> decimal`` = getGenericMethodInfo <@ decimal @>
    let ``-> float``   = getGenericMethodInfo <@ float @>
    let ``-> float32`` = getGenericMethodInfo <@ float32 @>
    let ``-> sbyte``   = getGenericMethodInfo <@ sbyte @>
    let ``-> int16``   = getGenericMethodInfo <@ int16 @>
    let ``-> int32``   = getGenericMethodInfo <@ int32 @>
    let ``-> int``     = getGenericMethodInfo <@ int @>
    let ``-> int64``   = getGenericMethodInfo <@ int64 @>
    let ``-> byte``    = getGenericMethodInfo <@ byte @>
    let ``-> uint16``  = getGenericMethodInfo <@ uint16 @>
    let ``-> uint32``  = getGenericMethodInfo <@ uint32 @>
    let ``-> uint64``  = getGenericMethodInfo <@ uint64 @>

    let ``-> checked.char``   = getGenericMethodInfo <@ Checked.char @>
    let ``-> checked.sbyte``  = getGenericMethodInfo <@ Checked.sbyte @>
    let ``-> checked.int16``  = getGenericMethodInfo <@ Checked.int16 @>
    let ``-> checked.int32``  = getGenericMethodInfo <@ Checked.int32 @>
    let ``-> checked.int64``  = getGenericMethodInfo <@ Checked.int64 @>
    let ``-> checked.byte``   = getGenericMethodInfo <@ Checked.byte @>
    let ``-> checked.uint16`` = getGenericMethodInfo <@ Checked.uint16 @>
    let ``-> checked.uint32`` = getGenericMethodInfo <@ Checked.uint32 @>
    let ``-> checked.uint64`` = getGenericMethodInfo <@ Checked.uint64 @>

    let ``-> linqExpressionHelper`` = getGenericMethodInfo <@ LinqExpressionHelper @>

    let ``-> getArray`` = getGenericMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.GetArray : int[] -> int -> int @>
    let ``-> setArray`` = getGenericMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.SetArray : int[] -> int -> int -> unit @>

    let (|ArrayTypeQ|_|) (ty:System.Type) = if ty.IsArray && ty.GetArrayRank() = 1 then Some(ty.GetElementType()) else None
    
    /// Convert F# quotations to LINQ expression trees.
    /// A more polished LINQ-Quotation translator will be published
    /// concert with later versions of LINQ.
    let rec ConvExpr (env:ConvEnv) (inp:Expr) = 
       //printf "ConvExpr : %A\n" e;
        match inp with 

        // Generic cases 
        | Patterns.Var(v) -> 
                try
                    Map.find v env.varEnv
                with
                |   :? KeyNotFoundException when v.Name = "this" ->
                        let message = 
                            "Encountered unxpected variable named 'this'. This might happen because " +
                            "quotations used in queries can’t contain references to let-bound values in classes unless the quotation literal occurs in an instance member. " +
                            "If this is the case, workaround by replacing references to implicit fields with references to " +
                            "local variables, e.g. rewrite\r\n" +
                            "   type Foo() =\r\n" +
                            "       let x = 1\r\n" +
                            "       let bar() = <@ x @>\r\n" +
                            "as: \r\n" +
                            "   type Foo() =\r\n" +
                            "       let x = 1\r\n" +
                            "       let bar() = let x = x in <@ x @>\r\n";

                        NotSupportedException(message) |> raise    
        | DerivedPatterns.AndAlso(x1,x2)             -> Expression.AndAlso(ConvExpr env x1, ConvExpr env x2) |> asExpr
        | DerivedPatterns.OrElse(x1,x2)              -> Expression.OrElse(ConvExpr env x1, ConvExpr env x2)  |> asExpr
        | Patterns.Value(x,ty)                -> Expression.Constant(x,ty)              |> asExpr

        // REVIEW: exact F# semantics for TypeAs and TypeIs
        | Patterns.Coerce(x,toTy)             -> Expression.TypeAs(ConvExpr env x,toTy)     |> asExpr
        | Patterns.TypeTest(x,toTy)           -> Expression.TypeIs(ConvExpr env x,toTy)     |> asExpr
        
        // Expr.*Get
        | Patterns.FieldGet(objOpt,fieldInfo) -> 
            Expression.Field(ConvObjArg env objOpt None, fieldInfo) |> asExpr

        | Patterns.TupleGet(arg,n) -> 
             let argP = ConvExpr env arg 
             let rec build ty argP n = 
                 match Reflection.FSharpValue.PreComputeTuplePropertyInfo(ty,n) with 
                 | propInfo,None -> 
                     Expression.Property(argP, propInfo)  |> asExpr
                 | propInfo,Some(nestedTy,n2) -> 
                     build nestedTy (Expression.Property(argP,propInfo) |> asExpr) n2
             build arg.Type argP n
              
        | Patterns.PropertyGet(objOpt,propInfo,args) -> 
            let coerceTo = 
                if objOpt.IsSome && FSharpType.IsUnion propInfo.DeclaringType && FSharpType.IsUnion propInfo.DeclaringType.BaseType  then  
                    Some propInfo.DeclaringType
                else 
                    None
            match args with 
            | [] -> 
                Expression.Property(ConvObjArg env objOpt coerceTo, propInfo) |> asExpr
            | _ -> 
                let argsP = ConvExprs env args
                Expression.Call(ConvObjArg env objOpt coerceTo, propInfo.GetGetMethod(true),argsP) |> asExpr

        // Expr.*Set
        | Patterns.PropertySet(objOpt,propInfo,args,v) -> 
            let args = (args @ [v])
            let argsP = ConvExprs env args 
            let minfo = propInfo.GetSetMethod(true)
            Expression.Call(ConvObjArg env objOpt None, minfo,argsP) |> wrapVoid

        // Expr.(Call,Application)
        | Patterns.Call(objOpt,minfo,args) -> 
            let transComparison x1 x2 exprConstructor exprErasedConstructor (intrinsic : MethodInfo) =
                let e1 = ConvExpr env x1
                let e2 = ConvExpr env x2

                if e1.Type.IsPrimitive || env.eraseEquality then
                    exprErasedConstructor(e1,e2) |> asExpr
                else 
                    exprConstructor(e1, e2, false, intrinsic.MakeGenericMethod([|x1.Type|])) |> asExpr

            match inp with 
//            | SpecificCall <@ ( .. ) @> (_,_,[x1;x2]) -> transComparison x1 x2 Expression.Equal              Expression.Equal              genericEqualityIntrinsic

            | Λ ``-> generic=`` (_,_,[x1;x2])
            | Λ ``-> =``  (_,_,[x1;x2]) -> transComparison x1 x2 Expression.Equal              Expression.Equal              genericEqualityIntrinsic
            | Λ ``-> >``  (_,_,[x1;x2]) -> transComparison x1 x2 Expression.GreaterThan        Expression.GreaterThan        genericGreaterThanIntrinsic
            | Λ ``-> >=`` (_,_,[x1;x2]) -> transComparison x1 x2 Expression.GreaterThanOrEqual Expression.GreaterThanOrEqual genericGreaterOrEqualIntrinsic
            | Λ ``-> <``  (_,_,[x1;x2]) -> transComparison x1 x2 Expression.LessThan           Expression.LessThan           genericLessThanIntrinsic
            | Λ ``-> <=`` (_,_,[x1;x2]) -> transComparison x1 x2 Expression.LessThanOrEqual    Expression.LessThanOrEqual    genericLessOrEqualIntrinsic
            | Λ ``-> <>`` (_,_,[x1;x2]) -> transComparison x1 x2 Expression.NotEqual           Expression.NotEqual           genericNotEqualIntrinsic

            | Λ ``-> not`` (_,_,[x1])    -> Expression.Not(ConvExpr env x1)                                   |> asExpr
            | Λ ``-> ~-``  (_,_,[x1])    -> Expression.Negate(ConvExpr env x1)                                |> asExpr
            | Λ ``-> +``   (_,_,[x1;x2]) -> Expression.Add(ConvExpr env x1, ConvExpr env x2)      |> asExpr
            | Λ ``-> /``   (_,_,[x1;x2]) -> Expression.Divide (ConvExpr env x1, ConvExpr env x2)  |> asExpr
            | Λ ``-> -``   (_,_,[x1;x2]) -> Expression.Subtract(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> *``   (_,_,[x1;x2]) -> Expression.Multiply(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> %``   (_,_,[x1;x2]) -> Expression.Modulo (ConvExpr env x1, ConvExpr env x2) |> asExpr
                 /// REVIEW: basic arithmetic with method witnesses
                 /// REVIEW: negate,add, divide, multiply, subtract with method witness

            | Λ ``-> <<<`` (_,_,[x1;x2]) -> Expression.LeftShift(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> >>>`` (_,_,[x1;x2]) -> Expression.RightShift(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> &&&`` (_,_,[x1;x2]) -> Expression.And(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> |||`` (_,_,[x1;x2]) -> Expression.Or(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> ^^^`` (_,_,[x1;x2]) -> Expression.ExclusiveOr(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> ~~~`` (_,_,[x1]) -> Expression.Not(ConvExpr env x1) |> asExpr
                 /// REVIEW: bitwise operations with method witnesses

            | Λ ``-> checked~-`` (_, _,[x1]) -> Expression.NegateChecked(ConvExpr env x1)                                |> asExpr
            | Λ ``-> checked+`` (_, _,[x1;x2]) -> Expression.AddChecked(ConvExpr env x1, ConvExpr env x2)      |> asExpr
            | Λ ``-> checked-`` (_, _,[x1;x2]) -> Expression.SubtractChecked(ConvExpr env x1, ConvExpr env x2) |> asExpr
            | Λ ``-> checked*`` (_, _,[x1;x2]) -> Expression.MultiplyChecked(ConvExpr env x1, ConvExpr env x2) |> asExpr

            | Λ ``-> char``    (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<char>) |> asExpr
            | Λ ``-> decimal`` (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<decimal>) |> asExpr
            | Λ ``-> float``   (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<float>) |> asExpr
            | Λ ``-> float32`` (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<float32>) |> asExpr
            | Λ ``-> sbyte``   (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<sbyte>) |> asExpr
            | Λ ``-> int16``   (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<int16>) |> asExpr
            | Λ ``-> int32``   (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<int32>) |> asExpr
            | Λ ``-> int``     (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<int32>) |> asExpr
            | Λ ``-> int64``   (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<int64>) |> asExpr
            | Λ ``-> byte``    (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<byte>) |> asExpr
            | Λ ``-> uint16``  (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<uint16>) |> asExpr
            | Λ ``-> uint32``  (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<uint32>) |> asExpr
            | Λ ``-> uint64``  (_, [|ty|],[x1]) -> Expression.Convert(ConvExpr env x1, typeof<uint64>) |> asExpr
             /// REVIEW: convert with method witness

            | Λ ``-> checked.char``   (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<char>) |> asExpr
            | Λ ``-> checked.sbyte``  (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<sbyte>) |> asExpr
            | Λ ``-> checked.int16``  (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<int16>) |> asExpr
            | Λ ``-> checked.int32``  (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<int32>) |> asExpr
            | Λ ``-> checked.int64``  (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<int64>) |> asExpr
            | Λ ``-> checked.byte``   (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<byte>) |> asExpr
            | Λ ``-> checked.uint16`` (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<uint16>) |> asExpr
            | Λ ``-> checked.uint32`` (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<uint32>) |> asExpr
            | Λ ``-> checked.uint64`` (_, [|ty|],[x1]) -> Expression.ConvertChecked(ConvExpr env x1, typeof<uint64>) |> asExpr

            | Λ ``-> getArray``  (_, [|ArrayTypeQ(elemTy);_;_|],[x1;x2]) -> 
                Expression.ArrayIndex(ConvExpr env x1, ConvExpr env x2) |> asExpr

            | Λ ``-> setArray``  (_, [|ArrayTypeQ(elemTy);_;_|],[arr;idx;elem]) -> 
                let minfo = ArrayAssignMethod.GetGenericMethodDefinition().MakeGenericMethod [| elemTy;typeof<unit> |]
                Expression.Call(minfo,[| ConvExpr env arr; ConvExpr env idx; ConvExpr env elem |]) |> asExpr
            
            // Throw away markers inserted to satisfy C#'s design where they pass an argument
            // or type T to an argument expecting Expr<T>.
            | Λ ``-> linqExpressionHelper`` (_, [|_|],[x1]) -> ConvExpr env x1
             
              /// ArrayLength
              /// ListBind
              /// ListInit
              /// ElementInit
            | _ -> 
                let argsP = ConvExprs env args 
                Expression.Call(ConvObjArg env objOpt None, minfo, argsP) |> wrapVoid

        // f x1 x2 x3 x4 --> InvokeFast4
        | Patterns.Application(Patterns.Application(Patterns.Application(Patterns.Application(f,arg1),arg2),arg3),arg4) -> 
            let domainTy1, rangeTy = getFunctionType f.Type
            let domainTy2, rangeTy = getFunctionType rangeTy
            let domainTy3, rangeTy = getFunctionType rangeTy
            let domainTy4, rangeTy = getFunctionType rangeTy
            let (-->) ty1 ty2 = Reflection.FSharpType.MakeFunctionType(ty1,ty2)
            let ty = domainTy1 --> domainTy2 
            let meth = (ty.GetMethods() |> Array.find (fun minfo -> minfo.Name = "InvokeFast" && minfo.GetParameters().Length = 5)).MakeGenericMethod [| domainTy3; domainTy4; rangeTy |]
            let argsP = ConvExprs env [f; arg1;arg2;arg3; arg4]
            Expression.Call((null:Expression), meth, argsP) |> asExpr

        // f x1 x2 x3 --> InvokeFast3
        | Patterns.Application(Patterns.Application(Patterns.Application(f,arg1),arg2),arg3) -> 
            let domainTy1, rangeTy = getFunctionType f.Type
            let domainTy2, rangeTy = getFunctionType rangeTy
            let domainTy3, rangeTy = getFunctionType rangeTy
            let (-->) ty1 ty2 = Reflection.FSharpType.MakeFunctionType(ty1,ty2)
            let ty = domainTy1 --> domainTy2 
            let meth = (ty.GetMethods() |> Array.find (fun minfo -> minfo.Name = "InvokeFast" && minfo.GetParameters().Length = 4)).MakeGenericMethod [| domainTy3; rangeTy |]
            let argsP = ConvExprs env [f; arg1;arg2;arg3]
            Expression.Call((null:Expression), meth, argsP) |> asExpr

        // f x1 x2 --> InvokeFast2
        | Patterns.Application(Patterns.Application(f,arg1),arg2) -> 
            let domainTy1, rangeTy = getFunctionType f.Type
            let domainTy2, rangeTy = getFunctionType rangeTy
            let (-->) ty1 ty2 = Reflection.FSharpType.MakeFunctionType(ty1,ty2)
            let ty = domainTy1 --> domainTy2 
            let meth = (ty.GetMethods() |> Array.find (fun minfo -> minfo.Name = "InvokeFast" && minfo.GetParameters().Length = 3)).MakeGenericMethod [| rangeTy |]
            let argsP = ConvExprs env [f; arg1;arg2]
            Expression.Call((null:Expression), meth, argsP) |> asExpr

        // f x1 --> Invoke
        | Patterns.Application(f,arg) -> 
            let fP = ConvExpr env f
            let argP = ConvExpr env arg
            let meth = f.Type.GetMethod("Invoke")
            Expression.Call(fP, meth, [| argP |]) |> asExpr

        // Expr.New*
        | Patterns.NewRecord(recdTy,args) -> 
            let ctorInfo = Reflection.FSharpValue.PreComputeRecordConstructorInfo(recdTy,showAll) 
            Expression.New(ctorInfo,ConvExprs env args) |> asExpr

        | Patterns.NewArray(ty,args) -> 
            Expression.NewArrayInit(ty,ConvExprs env args) |> asExpr

        | Patterns.DefaultValue(ty) -> 
            Expression.New(ty) |> asExpr

        | Patterns.NewUnionCase(unionCaseInfo,args) -> 
            let methInfo = Reflection.FSharpValue.PreComputeUnionConstructorInfo(unionCaseInfo,showAll)
            let argsR = ConvExprs env args 
            Expression.Call((null:Expression),methInfo,argsR) |> asExpr

        | Patterns.UnionCaseTest(e,unionCaseInfo) -> 
            let methInfo = Reflection.FSharpValue.PreComputeUnionTagMemberInfo(unionCaseInfo.DeclaringType,showAll)
            let obj = ConvExpr env e 
            let tagE = 
                match methInfo with 
                | :? PropertyInfo as p -> 
                    Expression.Property(obj,p) |> asExpr
                | :? MethodInfo as m -> 
                    Expression.Call((null:Expression),m,[| obj |]) |> asExpr
                | _ -> failwith "unreachable case"
            Expression.Equal(tagE, Expression.Constant(unionCaseInfo.Tag)) |> asExpr

        | Patterns.NewObject(ctorInfo,args) -> 
            Expression.New(ctorInfo,ConvExprs env args) |> asExpr

        | Patterns.NewDelegate(dty,vs,b) -> 
            let vsP = List.map ConvVar vs 
            let env = {env with varEnv = List.foldBack2 (fun (v:Var) vP -> Map.add v (vP |> asExpr)) vs vsP env.varEnv }
            let bodyP = ConvExpr env b
            Expression.Lambda(dty, bodyP, vsP) |> asExpr 

        | Patterns.NewTuple(args) -> 
             let tupTy = args |> List.map (fun arg -> arg.Type) |> Array.ofList |> Reflection.FSharpType.MakeTupleType
             let argsP = ConvExprs env args 
             let rec build ty (argsP: Expression[]) = 
                 match Reflection.FSharpValue.PreComputeTupleConstructorInfo(ty) with 
                 | ctorInfo,None -> Expression.New(ctorInfo,argsP) |> asExpr 
                 | ctorInfo,Some(nestedTy) -> 
                     let n = ctorInfo.GetParameters().Length - 1
                     Expression.New(ctorInfo, Array.append argsP.[0..n-1] [| build nestedTy argsP.[n..] |]) |> asExpr
             build tupTy argsP

        | Patterns.IfThenElse(g,t,e) -> 
            match e with
            | Patterns.Value(o,_) when o = null -> Expression.IfThen (ConvExpr env g, ConvExpr env t)|> asExpr
            | _ -> Expression.Condition(ConvExpr env g, ConvExpr env t,ConvExpr env e) |> asExpr

        | Patterns.Sequential (e1,e2) -> 
            let e1P = ConvExpr env e1
            let e2P = ConvExpr env e2
            Expression.Block(e1P, e2P) |> asExpr

        | Patterns.Let (v,e,b) -> 
            let vP = Expression.Variable (v.Type, v.Name)
            let eP = ConvExpr env e
            let assign = Expression.Assign (vP, eP) |> asExpr

            let envInner = { env with varEnv = Map.add v (vP |> asExpr) env.varEnv } 
            let bodyP = ConvExpr envInner b 

            Expression.Block ([vP], [assign; bodyP]) |> asExpr

        | Patterns.VarSet (variable, value) ->
            let linqVariable = Map.find variable env.varEnv
            let linqValue = ConvExpr env value
            Expression.Assign (linqVariable, linqValue)|> asExpr

        | Patterns.Lambda(firstVar, firstBody) as lambda ->
            let rec getVars vars maybeBody = function
            | Lambda (v, body) -> getVars (v::vars) (Some body) body
            | _ -> List.rev vars, maybeBody.Value

            let vars, body = getVars [] None lambda

            let capturedVars =
                let parameterVars = Set vars

                body.GetFreeVars ()
                |> Seq.filter (fun freeVar -> not <| Set.contains freeVar parameterVars)
                |> Seq.sortBy (fun freeVar -> freeVar.Name)
                |> Seq.toList

            let varsCount = vars.Length
            if varsCount <= 5 && capturedVars.Length <= 8 then
                let stateType =
                    match capturedVars with
                    | []                                 -> typeof<Unit>
                    | v1::[]                             -> v1.Type
                    | v1::v2::[]                         -> typedefof<Tuple<_,_>>.            MakeGenericType(v1.Type,v2.Type)
                    | v1::v2::v3::[]                     -> typedefof<Tuple<_,_,_>>.          MakeGenericType(v1.Type,v2.Type,v3.Type)
                    | v1::v2::v3::v4::[]                 -> typedefof<Tuple<_,_,_,_>>.        MakeGenericType(v1.Type,v2.Type,v3.Type,v4.Type)
                    | v1::v2::v3::v4::v5::[]             -> typedefof<Tuple<_,_,_,_,_>>.      MakeGenericType(v1.Type,v2.Type,v3.Type,v4.Type,v5.Type)
                    | v1::v2::v3::v4::v5::v6::[]         -> typedefof<Tuple<_,_,_,_,_,_>>.    MakeGenericType(v1.Type,v2.Type,v3.Type,v4.Type,v5.Type,v6.Type)
                    | v1::v2::v3::v4::v5::v6::v7::[]     -> typedefof<Tuple<_,_,_,_,_,_,_>>.  MakeGenericType(v1.Type,v2.Type,v3.Type,v4.Type,v5.Type,v6.Type,v7.Type)
                    | v1::v2::v3::v4::v5::v6::v7::v8::[] -> typedefof<Tuple<_,_,_,_,_,_,_,_>>.MakeGenericType(v1.Type,v2.Type,v3.Type,v4.Type,v5.Type,v6.Type,v7.Type,v8.Type)
                    | _ -> failwith "Not currently supported"

                let stateParameter =
                    Expression.Parameter (stateType, "state")

                let stateEnvironment =
                    match capturedVars with
                    | [] -> []
                    | v1 :: [] -> [v1, stateParameter |> asExpr]
                    | _ ->
                        capturedVars
                        |> List.mapi (fun idx var -> var, Expression.Property (stateParameter, "Item" + (idx+1).ToString()) |> asExpr)

                let varParameters =
                    vars
                    |> List.map (fun var -> var, Expression.Parameter (var.Type, var.Name))

                let lambdaEnv =
                    { env with
                         varEnv =
                            let environmentVariables =
                                varParameters
                                |> List.map (fun (v,p) -> v, p |> asExpr)
                                |> List.append stateEnvironment

                            (env.varEnv, environmentVariables)
                            ||> List.fold (fun varEnv (var, parameter) ->
                                varEnv
                                |> Map.add var parameter) }

                let linqBody = ConvExpr lambdaEnv body

                let parameters = 
                    [ yield stateParameter
                      yield! varParameters |> List.map snd ]

                let linqLambda = Expression.Lambda (linqBody, parameters)

                let ``function`` = linqLambda.Compile ()
              
                let funcFSharp =
                    if   varsCount = 1 then typedefof<FuncFSharp<_,_,_>>
                    elif varsCount = 2 then typedefof<FuncFSharp<_,_,_,_>>
                    elif varsCount = 3 then typedefof<FuncFSharp<_,_,_,_,_>>
                    elif varsCount = 4 then typedefof<FuncFSharp<_,_,_,_,_,_>>
                    elif varsCount = 5 then typedefof<FuncFSharp<_,_,_,_,_,_,_>>
                    else failwith "Logic error"

                let parameterTypes =
                    [|  yield stateType
                        yield! varParameters |> List.map (fun (vars,_) -> vars.Type)
                        yield linqBody.Type |]
                  
                let ``type`` = funcFSharp.MakeGenericType parameterTypes

                let ``constructor`` = ``type``.GetConstructor [| ``function``.GetType (); stateType |]

                match capturedVars with
                | [] ->
                    let obj = ``constructor``.Invoke [| ``function``; null |]
                    Expression.Constant (obj) |> asExpr
                | v1 :: [] ->
                    let state = Map.find v1 env.varEnv
                    Expression.New (``constructor``, [Expression.Constant(``function``) |> asExpr; state]) |> asExpr
                | _ ->
                    let state =
                        capturedVars
                        |> List.map (fun var -> Map.find var env.varEnv)

                    let stateConstructor =
                        let types = 
                            capturedVars
                            |> List.map (fun var -> var.Type)
                            |> List.toArray

                        stateType.GetConstructor types

                    Expression.New (``constructor``, [Expression.Constant(``function``) |> asExpr; Expression.New(stateConstructor, state) |> asExpr]) |> asExpr
            else
                let v, body = firstVar, firstBody

                let vP = ConvVar v
                let env = { env with varEnv = Map.add v (vP |> asExpr) env.varEnv }
                let tyargs = [| v.Type; body.Type |]
                let bodyP = ConvExpr env body
                let convType = typedefof<System.Converter<obj,obj>>.MakeGenericType tyargs
                let convDelegate = Expression.Lambda(convType, bodyP, [| vP |]) |> asExpr
                Expression.Call(typeof<FuncConvert>,"ToFSharpFunc",tyargs,[| convDelegate |]) |> asExpr
    
        | Patterns.WhileLoop(condition, iteration) -> 
            let linqCondition = ConvExpr env condition
            let linqIteration = ConvExpr env iteration

            let breakLabel = Expression.Label ()
            let linqLoop =
                Expression.Loop (
                    Expression.Block (
                        Expression.IfThenElse (
                            linqCondition,
                            linqIteration,
                            Expression.Break breakLabel)),
                    breakLabel)
            
            linqLoop |> wrapVoid

        | Patterns.ForIntegerRangeLoop(indexer, lowerValue, upperValue, iteration) ->
            let linqLowerValue = ConvExpr env lowerValue
            let linqUpperValue = ConvExpr env upperValue
            let linqIndexer = Expression.Variable (linqLowerValue.Type, indexer.Name)
            let linqAssignLower = Expression.Assign (linqIndexer, linqLowerValue)
            let linqCondition = Expression.LessThanOrEqual (linqIndexer, linqUpperValue)
            
            let envInner = { env with varEnv = Map.add indexer (linqIndexer |> asExpr) env.varEnv }

            let linqIteration = 
                Expression.Block (
                    ConvExpr envInner iteration,
                    Expression.Assign(linqIndexer, Expression.Increment (linqIndexer)))

            let breakLabel = Expression.Label ()
            let linqLoop =
                Expression.Loop (
                    Expression.Block (
                        Expression.IfThenElse (
                            linqCondition,
                            linqIteration,
                            Expression.Break breakLabel)),
                    breakLabel)

            let linqStatements =
                Expression.Block (
                    [linqIndexer],
                    [linqAssignLower |> asExpr; linqLoop |> asExpr]
                )

            linqStatements |> asExpr
        
        | Patterns.TryFinally(e,h) -> 
            let eP = ConvExpr env e
            let hP = ConvExpr env h
            Expression.TryFinally(eP, hP) |> asExpr
        
        | Patterns.TryWith(e,vf,filter,vh,handler) -> 
            let eP = ConvExpr env (Expr.Lambda(new Var("unitVar",typeof<unit>), e))
            let filterP = ConvExpr env (Expr.Lambda(vf,filter))
            let handlerP = ConvExpr env (Expr.Lambda(vh,handler))
            let minfo = TryWithMethod.GetGenericMethodDefinition().MakeGenericMethod [| e.Type |]
            Expression.Call(minfo,[| eP; filterP; handlerP |]) |> asExpr

        | Patterns.LetRecursive(binds,body) -> 

            let vfs = List.map fst binds
            
            let pass1 = 
                binds |> List.map (fun (vf,expr) -> 
                    match expr with 
                    | Lambda(vx,expr) -> 
                        let domainTy,rangeTy = getFunctionType vf.Type
                        let vfdTy = GetFuncType [| domainTy; rangeTy |]
                        let vfd = new Var("d",vfdTy)
                        (vf,vx,expr,domainTy,rangeTy,vfdTy,vfd)
                    | _ -> failwith "cannot convert recursive bindings that do not define functions")

            let trans = pass1 |> List.map (fun (vf,vx,expr,domainTy,rangeTy,vfdTy,vfd) -> (vf,vfd)) |> Map.ofList

            // Rewrite uses of the recursively defined functions to be invocations of the delegates
            // We do this because the delegate are allocated "once" and we can normally just invoke them efficiently
            let rec rw t = 
                match t with 
                | Application(Var(vf),t) when trans.ContainsKey(vf) -> 
                     let vfd = trans.[vf]
                     Expr.Call(Expr.Var(vfd),vfd.Type.GetMethod("Invoke",instanceBindingFlags),[t])
                | ExprShape.ShapeVar(vf) when trans.ContainsKey(vf)-> 
                     let vfd = trans.[vf]
                     let nv = new Var("nv",fst(getFunctionType vf.Type)) 
                     Expr.Lambda(nv,Expr.Call(Expr.Var(vfd),vfd.Type.GetMethod("Invoke",instanceBindingFlags),[Expr.Var(nv)]))
                | ExprShape.ShapeVar(_) -> t
                | ExprShape.ShapeCombination(obj,args) -> ExprShape.RebuildShapeCombination(obj,List.map rw args)
                | ExprShape.ShapeLambda(v,arg) -> Expr.Lambda(v,rw arg)

            let vfdTys    = pass1 |> List.map (fun (vf,vx,expr,domainTy,rangeTy,vfdTy,vfd) -> vfdTy) |> Array.ofList
            let vfds      = pass1 |> List.map (fun (vf,vx,expr,domainTy,rangeTy,vfdTy,vfd) -> vfd)

            let FPs = 
                [| for (vf,vx,expr,domainTy,rangeTy,vfdTy,vfd) in pass1 do
                      let expr = rw expr
                      let tyF = GetFuncType (Array.append vfdTys [| vx.Type; expr.Type |])
                      let F = Expr.NewDelegate(tyF,vfds@[vx],expr)
                      let FP = ConvExpr env F
                      yield FP |]

            let body = rw body

            let methTys   = 
                [| for (vf,vx,expr,domainTy,rangeTy,vfdTy,vfd) in pass1 do
                      yield domainTy
                      yield rangeTy
                   yield body.Type |]

            let B = Expr.NewDelegate(GetFuncType (Array.append vfdTys [| body.Type |]),vfds,body)
            let BP = ConvExpr env B

            let minfo = 
                let q = 
                    match vfds.Length with 
                    | 1 -> <@@ LetRec1Helper @@>
                    | 2 -> <@@ LetRec2Helper @@>
                    | 3 -> <@@ LetRec3Helper @@>
                    | 4 -> <@@ LetRec4Helper @@>
                    | 5 -> <@@ LetRec5Helper @@>
                    | 6 -> <@@ LetRec6Helper @@>
                    | 7 -> <@@ LetRec7Helper @@>
                    | 8 -> <@@ LetRec8Helper @@>
                    | _ -> raise <| new NotSupportedException("In this release of the F# Power Pack, mutually recursive function groups involving 9 or more functions may not be converted to LINQ expressions")
                match q with Lambdas(_,Call(_,minfo,_)) -> minfo | _ -> failwith "couldn't find minfo"

            let minfo = minfo.GetGenericMethodDefinition().MakeGenericMethod methTys
            Expression.Call(minfo,Array.append FPs [| BP |]) |> asExpr

        | Patterns.AddressOf _ -> raise <| new NotSupportedException("Address-of expressions may not be converted to LINQ expressions")
        | Patterns.AddressSet _ -> raise <| new NotSupportedException("Address-set expressions may not be converted to LINQ expressions")
        | Patterns.FieldSet _ -> raise <| new NotSupportedException("Field-set expressions may not be converted to LINQ expressions")

        | _ -> 
            raise <| new NotSupportedException(sprintf "Could not convert the following F# Quotation to a LINQ Expression Tree\n--------\n%A\n-------------\n" inp)

    and ConvObjArg env objOpt coerceTo : Expression = 
        match objOpt with
        | Some(obj) -> 
            let expr = ConvExpr env obj
            match coerceTo with 
            | None -> expr
            | Some ty -> Expression.TypeAs(expr, ty) :> Expression
        | None -> 
            null

    and ConvExprs env es : Expression[] = 
        es |> List.map (ConvExpr env) |> Array.ofList 

    and ConvVar (v: Var) = 
        //printf "** Expression .Parameter(%a, %a)\n" output_any ty output_any nm;
        Expression.Parameter(v.Type, v.Name)

    module ``Custom Enumerables`` =
        let CurrentIsState preStartState (moveNext:Func<_,_>) =
            let getEnumerator () =
                let current = ref preStartState
                { new IEnumerator<'a> with
                    member __.Dispose() =  ()
                    member __.Current : 'a = !current
                    member __.Current : obj = upcast (!current)
                    member __.MoveNext()  = 
                        match moveNext.Invoke (!current) with
                        | None -> false
                        | Some next -> current := next; true
                    member __.Reset() = current := preStartState }

            { new IEnumerable<'a> with
                member __.GetEnumerator() = getEnumerator ()
                member __.GetEnumerator(): Collections.IEnumerator = upcast (getEnumerator ()) }

    let inline IsType<'a,'b> = typeof<'a> = typeof<'b>

    [<Sealed>]
    type ``Custom Operators``<'a> private () =
        static let one =
            if   IsType<'a, sbyte>   then failwith "Not supported"
            elif IsType<'a, byte>    then failwith "Not supported"
            elif IsType<'a, int16>   then box 1s
            elif IsType<'a, uint16>  then box 1us
            elif IsType<'a, int32>   then box 1
            elif IsType<'a, uint32>  then box 1ul
            elif IsType<'a, int64>   then box 1L
            elif IsType<'a, uint64>  then box 1UL
            elif IsType<'a, float32> then box 1.f
            elif IsType<'a, float>   then box 1.
            elif IsType<'a, bigint>  then box 1I
            elif IsType<'a, decimal> then box 1M
            else failwith <| sprintf "Don't support type %A" typeof<'a>

        static let optionType = typeof<option<'a>>
        static let optionConstructor = optionType.GetConstructor [| typeof<'a> |]

        static let subtract =
            let n = Expression.Parameter (typeof<'a>, "n")
            let amount = Expression.Parameter (typeof<'a>, "amount")
            let subtractOne = Expression.Lambda<Func<'a,'a,'a>>( Expression.Subtract(n, amount), n, amount)
            subtractOne.Compile()

        static let boundedIncrement =
            let incr  = Expression.Parameter (typeof<'a>, "incr")
            let upper = Expression.Parameter (typeof<'a>, "upper")
            let n     = Expression.Parameter (typeof<'a>, "n")

            let next = Expression.Variable (typeof<'a>)
            let innerIncrement =
                Expression.Lambda(
                    Expression.Block(
                        [next],
                        [   Expression.Assign (next, Expression.Add(n, incr)) :> Expression
                            Expression.Condition (
                                Expression.LessThanOrEqual (next, upper),
                                Expression.New (optionConstructor, next),
                                Expression.Property (null, optionType, "None")) :> Expression ]),
                    n)

            let outerBounding =
                Expression.Lambda<Func<'a, 'a, Func<'a, option<'a>>>> (innerIncrement, incr, upper)

            outerBounding.Compile ()

        static member (..) lower upper =
            let one = one :?> 'a
            let preStartState = subtract.Invoke (lower, one)
            let moveNext = boundedIncrement.Invoke (one, upper)
            ``Custom Enumerables``.CurrentIsState preStartState moveNext

        static member (.. ..) lower (incr:'a) upper =
            let preStartState = subtract.Invoke (lower, incr)
            let moveNext = boundedIncrement.Invoke (incr, upper)
            ``Custom Enumerables``.CurrentIsState preStartState moveNext

    let ``-> id`` = getGenericMethodInfo <@ id @>
    let ``-> |>`` = getGenericMethodInfo <@ (|>) @>
    let ``-> <|`` = getGenericMethodInfo <@ (<|) @>
    let ``-> ..`` = getGenericMethodInfo <@ (..) @>
    let ``-> .. ..`` = getGenericMethodInfo <@ (.. ..) @>
    let ``-> Custom ..`` = getGenericMethodInfo <@ ``Custom Operators``.op_Range @>
    let ``-> Custom .. ..`` = getGenericMethodInfo <@ ``Custom Operators``.op_RangeStep @>

    let (|TraverseExpr|_|) f = function
    | ExprShape.ShapeCombination (o, exprlist) -> Some (ExprShape.RebuildShapeCombination (o, List.map f exprlist))
    | ExprShape.ShapeLambda (var, expr) -> Some (Expr.Lambda (var, f expr))
    | untouched -> Some untouched

    let rec constantReplacement var value = function
    | Patterns.Var v when v = var -> value
    | TraverseExpr (constantReplacement var value) result -> result
    | _ -> failwith "Invalid logic"

    let rec optimize = function
    | Patterns.Let (var, binding, body) when not var.IsMutable ->
        match optimize binding with
        | (Patterns.Value _) as value -> optimize <| constantReplacement var value body
        | optimizedBinding -> Expr.Let (var, optimizedBinding, optimize body)
    | Patterns.Application (Lambda(var, body), input) -> optimize <| Expr.Let (var, input, body)
    | Λ ``-> ..`` (None, [|``type``|], args) when ``type`` <> typeof<byte> && ``type`` <> typeof<sbyte> ->
        let customOperatorsType = typedefof<``Custom Operators``<_>>
        let specificType = customOperatorsType.MakeGenericType ``type``
        let ``method`` = specificType.GetMethod (``-> Custom ..``.Name, BindingFlags.NonPublic ||| BindingFlags.Static)
        optimize <| Expr.Call (``method``, args)
    | Λ ``-> .. ..`` (None, [|ty1;ty2|], args) when ty1 = ty2 && (ty1 <> typeof<byte> && ty1 <> typeof<sbyte>) ->
        let customOperatorsType = typedefof<``Custom Operators``<_>>
        let specificType = customOperatorsType.MakeGenericType ty1
        let ``method`` = specificType.GetMethod (``-> Custom .. ..``.Name, BindingFlags.NonPublic ||| BindingFlags.Static)
        optimize <| Expr.Call (``method``, args)
    | Λ ``-> |>`` (None, _, [x1;x2]) -> optimize <| Expr.Application (x2, x1)
    | Λ ``-> <|`` (None, _, [x1;x2]) -> optimize <| Expr.Application (x1, x2)
    | Λ ``-> +`` (None, [|t1;_;_|], [x1;x2]) when t1 = typeof<string> ->
        let rec getStrings strings = function
        | Λ ``-> +`` (None, [|t1;_;_|], [x1;x2]) when t1 = typeof<string> -> getStrings (x2::strings) (x1)
        | remainder -> remainder :: strings
        let concat = 
            match getStrings [x2] (x1) with
            | s1::s2::[]         -> <@@ String.Concat(%%s1, %%s2) @@>
            | s1::s2::s3::[]     -> <@@ String.Concat(%%s1, %%s2, %%s3) @@>
            | s1::s2::s3::s4::[] -> <@@ String.Concat(%%s1, %%s2, %%s3, %%s4) @@>
            | strings            -> Expr.Call (getMethodInfo <@ String.Concat ([||]:array<string>) @>, [Expr.NewArray (typeof<string>, strings)])
        optimize <| concat
    | Λ ``-> id`` (None, _, [x1]) -> optimize x1
    | TraverseExpr optimize result -> result
    | _ -> failwith "Invalid logic"

    let Conv (e: #Expr,eraseEquality) =
        let e = optimize e

        let linqExpr = ConvExpr { eraseEquality = eraseEquality; varEnv = Map.empty } (e :> Expr)

        Expression.Lambda(linqExpr, Expression.Parameter(typeof<unit>)) |> asExpr

    let CompileImpl (e: #Expr, eraseEquality) = 
//       let ty = e.Type
//       let e = Expr.NewDelegate(GetFuncType([|typeof<unit>; ty |]), [new Var("unit",typeof<unit>)],e)
       let linqExpr = Conv (e,eraseEquality)
       let linqExpr = (linqExpr :?> LambdaExpression)
       let d = linqExpr.Compile()
       (fun () -> 
           try 
             d.DynamicInvoke [| box () |]
           with :? System.Reflection.TargetInvocationException as exn -> 
               raise exn.InnerException)

    let Compile (e: #Expr) = CompileImpl(e,false)

    let Eval e = Compile e ()

module QuotationEvaluationExtensions =

    open QuotationEvaluationTypes

    type Microsoft.FSharp.Quotations.Expr with 
        member x.ToLinqExpressionUntyped() = Conv(x, false)
        member x.CompileUntyped() = 
            let f = Compile(x)  
            f() 
        member x.EvaluateUntyped() = Eval(x)

    type Microsoft.FSharp.Quotations.Expr<'T> with 
        member x.Compile() = 
            let f = Compile(x)  
            f() :?> 'T
        member x.Evaluate() = (Eval(x) :?> 'T)

  
open QuotationEvaluationTypes
open QuotationEvaluationExtensions
  
[<Sealed>]
type QuotationEvaluator() = 

    static member ToLinqExpression (e: Microsoft.FSharp.Quotations.Expr) = e.ToLinqExpressionUntyped()

    static member CompileUntyped (e : Microsoft.FSharp.Quotations.Expr) = e.CompileUntyped()

    static member EvaluateUntyped (e : Microsoft.FSharp.Quotations.Expr) = e.EvaluateUntyped()

    static member internal EvaluateUntypedUsingQueryApproximations (e: Microsoft.FSharp.Quotations.Expr) = CompileImpl(e, true) ()

    static member Compile (e : Microsoft.FSharp.Quotations.Expr<'T>) = e.Compile()

    static member Evaluate (e : Microsoft.FSharp.Quotations.Expr<'T>) = e.Evaluate()

    