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

    type ConvEnv = {
        eraseEquality : bool
        varEnv        : Map<Var,Expression>
    }

    type ConvResult =
    | AsExpression of Expression
    | AsLetRecFunction of ParameterExpression * Expression * Expression
//    | AsLetRecRecord of ParameterExpression * Expression * Expression

    let asExpr x = AsExpression x
    let asExpression x = (x :> Expression)

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

    let TryWithHelper e filter handler = 
        try e() 
        with e when (filter e <> 0) -> handler e

    let ArrayAssignMethod = match <@@ ArrayAssignHelper @@> with Lambdas(_,Call(_,minfo,_)) -> minfo | _ -> failwith "couldn't find minfo"
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
    open Tools
    
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
            
    type FuncFSharp<'state,'a> (f:Func<'state,'a>) =
        inherit FSharpFunc<unit, 'a>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke _ = f.Invoke this.State

    type FuncFSharp<'state,'a,'b> (f:Func<'state,'a,'b>) =
        inherit FSharpFunc<'a,'b>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke a = f.Invoke (this.State,a)

    type FuncFSharp<'state,'a,'b,'c> (f:Func<'state,'a,'b,'c>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b) = f.Invoke (this.State,a,b)
        override this.Invoke a = fun b -> f.Invoke (this.State,a,b)

    type FuncFSharp<'state,'a,'b,'c,'d> (f:Func<'state,'a,'b,'c,'d>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c) = f.Invoke (this.State,a,b,c)
        override this.Invoke a = fun b c -> f.Invoke (this.State,a,b,c)

    type FuncFSharp<'state,'a,'b,'c,'d,'e> (f:Func<'state,'a,'b,'c,'d,'e>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d) = f.Invoke (this.State,a,b,c,d)
        override this.Invoke a = fun b c d -> f.Invoke (this.State,a,b,c,d)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f> (f:Func<'state,'a,'b,'c,'d,'e,'f>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = f.Invoke (this.State,a,b,c,d,e)
        override this.Invoke a = fun b c d e -> f.Invoke (this.State,a,b,c,d,e)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f -> func.Invoke (this.State,a,b,c,d,e,f)
        override this.Invoke a = fun b c d e f -> func.Invoke (this.State,a,b,c,d,e,f)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g -> func.Invoke (this.State,a,b,c,d,e,f,g)
        override this.Invoke a = fun b c d e f g -> func.Invoke (this.State,a,b,c,d,e,f,g)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h -> func.Invoke (this.State,a,b,c,d,e,f,g,h)
        override this.Invoke a = fun b c d e f g h -> func.Invoke (this.State,a,b,c,d,e,f,g,h)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h i -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i)
        override this.Invoke a = fun b c d e f g h i -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h i j -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j)
        override this.Invoke a = fun b c d e f g h i j -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h i j k -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k)
        override this.Invoke a = fun b c d e f g h i j k -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h i j k l -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l)
        override this.Invoke a = fun b c d e f g h i j k l -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h i j k l m -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m)
        override this.Invoke a = fun b c d e f g h i j k l m -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
        override this.Invoke a = fun b c d e f g h i j k l m n -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n)

    type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p>) =
        inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p>()
        [<Core.DefaultValue false>] val mutable State : 'state
        override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n o -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
        override this.Invoke a = fun b c d e f g h i j k l m n o -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

    let IsVoidType (ty:System.Type)  = (ty = typeof<System.Void>)

    let LinqExpressionHelper (x:'T) : Expression<'T> = failwith ""
    
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
        match LetRecConvExpr env None inp with
        | AsExpression expr -> expr
        | _ -> failwith "Invalid logic"
    and LetRecConvExpr (env:ConvEnv) (letrec:option<Var>) (inp:Expr) = 
       //printf "ConvExpr : %A\n" e;
        match inp with 

        // Generic cases 
        | Patterns.Var(v) -> 
                try
                    Map.find v env.varEnv |> asExpr
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
                     build nestedTy (Expression.Property(argP,propInfo) |> asExpression) n2
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
            let unary x1 f     = f (ConvExpr env x1) |> asExpr
            let binary x1 x2 f = f (ConvExpr env x1, ConvExpr env x2) |> asExpr

            let convert x1 t        = Expression.Convert (ConvExpr env x1, t) |> asExpr
            let convertChecked x1 t = Expression.Convert (ConvExpr env x1, t) |> asExpr

            let transComparison x1 x2 exprConstructor exprErasedConstructor (intrinsic : MethodInfo) =
                let e1 = ConvExpr env x1
                let e2 = ConvExpr env x2

                if e1.Type.IsPrimitive || env.eraseEquality then
                    exprErasedConstructor(e1,e2) |> asExpr
                else 
                    exprConstructor(e1, e2, false, intrinsic.MakeGenericMethod([|x1.Type|])) |> asExpr

            match inp with 
            | Λ ``-> generic=`` (_,_,[x1;x2])
            | Λ ``-> =``  (_,_,[x1;x2]) -> transComparison x1 x2 Expression.Equal              Expression.Equal              genericEqualityIntrinsic
            | Λ ``-> >``  (_,_,[x1;x2]) -> transComparison x1 x2 Expression.GreaterThan        Expression.GreaterThan        genericGreaterThanIntrinsic
            | Λ ``-> >=`` (_,_,[x1;x2]) -> transComparison x1 x2 Expression.GreaterThanOrEqual Expression.GreaterThanOrEqual genericGreaterOrEqualIntrinsic
            | Λ ``-> <``  (_,_,[x1;x2]) -> transComparison x1 x2 Expression.LessThan           Expression.LessThan           genericLessThanIntrinsic
            | Λ ``-> <=`` (_,_,[x1;x2]) -> transComparison x1 x2 Expression.LessThanOrEqual    Expression.LessThanOrEqual    genericLessOrEqualIntrinsic
            | Λ ``-> <>`` (_,_,[x1;x2]) -> transComparison x1 x2 Expression.NotEqual           Expression.NotEqual           genericNotEqualIntrinsic

            | Λ ``-> not`` (_,_,[x1])    -> unary  x1    Expression.Not
            | Λ ``-> ~-``  (_,_,[x1])    -> unary  x1    Expression.Negate
            | Λ ``-> +``   (_,_,[x1;x2]) -> binary x1 x2 Expression.Add
            | Λ ``-> /``   (_,_,[x1;x2]) -> binary x1 x2 Expression.Divide
            | Λ ``-> -``   (_,_,[x1;x2]) -> binary x1 x2 Expression.Subtract
            | Λ ``-> *``   (_,_,[x1;x2]) -> binary x1 x2 Expression.Multiply
            | Λ ``-> %``   (_,_,[x1;x2]) -> binary x1 x2 Expression.Modulo
                 /// REVIEW: basic arithmetic with method witnesses
                 /// REVIEW: negate,add, divide, multiply, subtract with method witness

            | Λ ``-> <<<`` (_,_,[x1;x2]) -> binary x1 x2 Expression.LeftShift
            | Λ ``-> >>>`` (_,_,[x1;x2]) -> binary x1 x2 Expression.RightShift
            | Λ ``-> &&&`` (_,_,[x1;x2]) -> binary x1 x2 Expression.And
            | Λ ``-> |||`` (_,_,[x1;x2]) -> binary x1 x2 Expression.Or
            | Λ ``-> ^^^`` (_,_,[x1;x2]) -> binary x1 x2 Expression.ExclusiveOr
            | Λ ``-> ~~~`` (_,_,[x1])    -> unary  x1    Expression.Not
                 /// REVIEW: bitwise operations with method witnesses

            | Λ ``-> checked~-`` (_,_,[x1])    -> unary  x1    Expression.NegateChecked
            | Λ ``-> checked+``  (_,_,[x1;x2]) -> binary x1 x2 Expression.AddChecked
            | Λ ``-> checked-``  (_,_,[x1;x2]) -> binary x1 x2 Expression.SubtractChecked
            | Λ ``-> checked*``  (_,_,[x1;x2]) -> binary x1 x2 Expression.MultiplyChecked
             
            | Λ ``-> char``    (_,_,[x1]) -> convert x1 typeof<char>
            | Λ ``-> decimal`` (_,_,[x1]) -> convert x1 typeof<decimal>
            | Λ ``-> float``   (_,_,[x1]) -> convert x1 typeof<float>
            | Λ ``-> float32`` (_,_,[x1]) -> convert x1 typeof<float32>
            | Λ ``-> sbyte``   (_,_,[x1]) -> convert x1 typeof<sbyte>
            | Λ ``-> int16``   (_,_,[x1]) -> convert x1 typeof<int16>
            | Λ ``-> int32``   (_,_,[x1]) -> convert x1 typeof<int32>
            | Λ ``-> int``     (_,_,[x1]) -> convert x1 typeof<int32>
            | Λ ``-> int64``   (_,_,[x1]) -> convert x1 typeof<int64>
            | Λ ``-> byte``    (_,_,[x1]) -> convert x1 typeof<byte>
            | Λ ``-> uint16``  (_,_,[x1]) -> convert x1 typeof<uint16>
            | Λ ``-> uint32``  (_,_,[x1]) -> convert x1 typeof<uint32>
            | Λ ``-> uint64``  (_,_,[x1]) -> convert x1 typeof<uint64>
             /// REVIEW: convert with method witness

            | Λ ``-> checked.char``   (_,_,[x1]) -> convertChecked x1 typeof<char>
            | Λ ``-> checked.sbyte``  (_,_,[x1]) -> convertChecked x1 typeof<sbyte>
            | Λ ``-> checked.int16``  (_,_,[x1]) -> convertChecked x1 typeof<int16>
            | Λ ``-> checked.int32``  (_,_,[x1]) -> convertChecked x1 typeof<int32>
            | Λ ``-> checked.int64``  (_,_,[x1]) -> convertChecked x1 typeof<int64>
            | Λ ``-> checked.byte``   (_,_,[x1]) -> convertChecked x1 typeof<byte>
            | Λ ``-> checked.uint16`` (_,_,[x1]) -> convertChecked x1 typeof<uint16>
            | Λ ``-> checked.uint32`` (_,_,[x1]) -> convertChecked x1 typeof<uint32>
            | Λ ``-> checked.uint64`` (_,_,[x1]) -> convertChecked x1 typeof<uint64>

            | Λ ``-> getArray``  (_, [|ArrayTypeQ(elemTy);_;_|],[x1;x2]) -> 
                Expression.ArrayIndex(ConvExpr env x1, ConvExpr env x2) |> asExpr

            | Λ ``-> setArray``  (_, [|ArrayTypeQ(elemTy);_;_|],[arr;idx;elem]) -> 
                let minfo = ArrayAssignMethod.GetGenericMethodDefinition().MakeGenericMethod [| elemTy;typeof<unit> |]
                Expression.Call(minfo,[| ConvExpr env arr; ConvExpr env idx; ConvExpr env elem |]) |> asExpr
            
            // Throw away markers inserted to satisfy C#'s design where they pass an argument
            // or type T to an argument expecting Expr<T>.
            | Λ ``-> linqExpressionHelper`` (_, [|_|],[x1]) -> LetRecConvExpr env letrec x1
             
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
                    Expression.Property(obj,p) |> asExpression
                | :? MethodInfo as m -> 
                    Expression.Call((null:Expression),m,[| obj |]) |> asExpression
                | _ -> failwith "unreachable case"
            Expression.Equal(tagE, Expression.Constant(unionCaseInfo.Tag)) |> asExpr

        | Patterns.NewObject(ctorInfo,args) -> 
            Expression.New(ctorInfo,ConvExprs env args) |> asExpr

        | Patterns.NewDelegate(dty,vs,b) -> 
            let vsP = List.map ConvVar vs 
            let env = {env with varEnv = List.foldBack2 (fun (v:Var) vP -> Map.add v (vP |> asExpression)) vs vsP env.varEnv }
            let bodyP = ConvExpr env b
            Expression.Lambda(dty, bodyP, vsP) |> asExpr 

        | Patterns.NewTuple(args) -> 
             let tupTy = args |> List.map (fun arg -> arg.Type) |> Array.ofList |> Reflection.FSharpType.MakeTupleType
             let argsP = ConvExprs env args 
             let rec build ty (argsP: Expression[]) = 
                 match Reflection.FSharpValue.PreComputeTupleConstructorInfo(ty) with 
                 | ctorInfo,None -> Expression.New(ctorInfo,argsP) |> asExpression 
                 | ctorInfo,Some(nestedTy) -> 
                     let n = ctorInfo.GetParameters().Length - 1
                     Expression.New(ctorInfo, Array.append argsP.[0..n-1] [| build nestedTy argsP.[n..] |]) |> asExpression
             build tupTy argsP |> asExpr

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
            let assign = Expression.Assign (vP, eP) |> asExpression

            let env = { env with varEnv = env.varEnv |> Map.add v (vP |> asExpression) } 
            let bodyP = ConvExpr env b 

            Expression.Block ([vP], [assign; bodyP]) |> asExpr

        | Patterns.VarSet (variable, value) ->
            let linqVariable = Map.find variable env.varEnv
            let linqValue = ConvExpr env value
            Expression.Assign (linqVariable, linqValue)|> asExpr

        | Patterns.Lambda (firstVar, firstBody) as lambda ->
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
            if varsCount <= 15 then
                let stateType, makeStateConstructor =
                    match capturedVars |> List.map (fun v -> v.Type) |> List.toArray with
                    | [|t|] -> t, (fun x -> Seq.head x)
                    | types -> createGenericTupleType types

                let stateParameter =
                    Expression.Parameter (stateType, "capturedState")

                let stateEnvironment =
                    match capturedVars with
                    | v1 :: [] -> [v1, stateParameter |> asExpression]
                    | _ -> List.mapi (fun idx var -> var, getExpressionFromTuple stateParameter idx) capturedVars

                let varParameters =
                    vars
                    |> List.map (fun var -> var, Expression.Parameter (var.Type, var.Name))

                let lambdaEnv =
                    { env with
                         varEnv =
                            let environmentVariables =
                                varParameters
                                |> List.map (fun (v,p) -> v, p |> asExpression)
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
                    match varsCount with
                    | 1  -> typedefof<FuncFSharp<_,_,_>>
                    | 2  -> typedefof<FuncFSharp<_,_,_,_>>
                    | 3  -> typedefof<FuncFSharp<_,_,_,_,_>>
                    | 4  -> typedefof<FuncFSharp<_,_,_,_,_,_>>
                    | 5  -> typedefof<FuncFSharp<_,_,_,_,_,_,_>>
                    | 6  -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_>>
                    | 7  -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_>>
                    | 8  -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_>>
                    | 9  -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_>>
                    | 10 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_>>
                    | 11 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_>>
                    | 12 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
                    | 13 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
                    | 14 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
                    | 15 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
                    | _ -> failwith "Logic error"

                let parameterTypes =
                    [|  yield stateType
                        yield! varParameters |> List.map (fun (vars,_) -> vars.Type)
                        yield linqBody.Type |]
                  
                let ``type`` = funcFSharp.MakeGenericType parameterTypes

                let ``constructor`` = ``type``.GetConstructor [| ``function``.GetType () |]

                let theFuncObject = Expression.Variable (``type``, "funcObject")

                match capturedVars with
                | [] ->
                    let obj = ``constructor``.Invoke [| ``function`` |]
                    Expression.Constant obj |> asExpr
                | _ ->
                    let newObject = 
                        Expression.New (
                            ``constructor``,
                            [Expression.Constant(``function``) |> asExpression])

                    let state =
                        let getVar var =
                            if Some var = letrec
                                then theFuncObject |> asExpression
                                else Map.find var env.varEnv

                        match capturedVars with
                        | v1 :: [] -> getVar v1
                        | _ ->
                            capturedVars
                            |> List.map getVar
                            |> makeStateConstructor

                    let assignToConstruction =
                        Expression.Assign(
                            theFuncObject,
                            newObject) |> asExpression;

                    let assignState = 
                        Expression.Assign(
                            Expression.PropertyOrField(theFuncObject, "State"),
                            state) |> asExpression;

                    if letrec.IsSome then
                        AsLetRecFunction (theFuncObject, assignToConstruction, assignState)
                    else
                        Expression.Block (
                            [ theFuncObject ],
                            [
                                assignToConstruction;
                                assignState;
                                theFuncObject |> asExpression
                            ]) |> asExpr
            else
                let v, body = firstVar, firstBody

                let vP = ConvVar v
                let env = { env with varEnv = Map.add v (vP |> asExpression) env.varEnv }
                let tyargs = [| v.Type; body.Type |]
                let bodyP = ConvExpr env body
                let convType = typedefof<System.Converter<obj,obj>>.MakeGenericType tyargs
                let convDelegate = Expression.Lambda(convType, bodyP, [| vP |]) |> asExpression
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
            
            let envInner = { env with varEnv = Map.add indexer (linqIndexer |> asExpression) env.varEnv }

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
                    [linqAssignLower |> asExpression; linqLoop |> asExpression]
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

        | Patterns.LetRecursive (binds, body) -> 
            let variablesAsLinq =
                binds
                |> List.map (fun (v, e) ->
                    v, Expression.Variable (v.Type, v.Name), e)

            let env = {
                env with
                    varEnv =
                        (env.varEnv, variablesAsLinq)
                        ||> List.fold (fun varEnv (v,vP,_) ->
                            varEnv
                            |> Map.add v (vP |> asExpression)) }

            let bindingAsLinq =
                variablesAsLinq
                |> List.map (fun (v, vP, e) -> vP, LetRecConvExpr env (Some v) e)

            let nonRecursiveBindings =
                bindingAsLinq
                |> List.choose (function
                    | vP, AsExpression e -> Some (vP, Expression.Assign(vP, e) |> asExpression)
                    | _ -> None)

            let nonRecursiveAssignments =
                nonRecursiveBindings
                |> List.map snd

            let variables =
                nonRecursiveBindings
                |> List.map fst

            let recursiveFunctionBindings =
                bindingAsLinq
                |> List.choose (function
                    | vP, AsLetRecFunction (funcObject, assignToFuncObject, assignState) ->
                        Some (vP, funcObject, assignToFuncObject, assignState)
                    | _ -> None)

            let variables =
                recursiveFunctionBindings
                |> List.collect (fun (v,fo,_,_) -> [ v; fo ])
                |> List.append variables

            let assignToLocalObject =
                recursiveFunctionBindings
                |> List.map (fun (_,_,assignToFuncObject,_) -> assignToFuncObject)

            let assignToFuncObject =
                recursiveFunctionBindings
                |> List.map (fun (vP, funcObject, _,_) ->
                    Expression.Assign (vP, funcObject) |> asExpression)
                
            let assignState =
                recursiveFunctionBindings
                |> List.map (fun (_,_,_, assignState) -> assignState)

            let bodyP = ConvExpr env body

            Expression.Block (
                variables,
                [
                    yield! nonRecursiveAssignments;
                    yield! assignToLocalObject;
                    yield! assignToFuncObject;
                    yield! assignState;
                    yield bodyP;
                ]) |> asExpr

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
    type OpRange() =
        static let methods =
            typeof<OpRange>.GetMethods (BindingFlags.Static ||| BindingFlags.NonPublic)
            |> Array.filter (fun ``method`` -> ``method``.Name = "op_Range")
            |> Array.map (fun ``method`` -> ``method``.ReturnType.GetGenericArguments().[0], ``method``)
            |> dict

        static member TypeProvided ``type`` = methods.ContainsKey ``type``
        static member GetMethod ``type`` = methods.[``type``]

        static member private (..) (lower, upper) : seq<byte>    = { lower .. upper}
        static member private (..) (lower, upper) : seq<sbyte>   = { lower .. upper}
        static member private (..) (lower, upper) : seq<int16>   = { lower .. upper}
        static member private (..) (lower, upper) : seq<uint16>  = { lower .. upper}
        static member private (..) (lower, upper) : seq<int32>   = { lower .. upper}
        static member private (..) (lower, upper) : seq<uint32>  = { lower .. upper}
        static member private (..) (lower, upper) : seq<int64>   = { lower .. upper}
        static member private (..) (lower, upper) : seq<uint64>  = { lower .. upper}
        static member private (..) (lower, upper) : seq<float32> = { lower .. upper}
        static member private (..) (lower, upper) : seq<float>   = { lower .. upper}
        static member private (..) (lower, upper) : seq<bigint>  = { lower .. upper}
        static member private (..) (lower, upper) : seq<decimal> = { lower .. upper}

    [<Sealed>]
    type OpRangeStep() =
        static let methods =
            typeof<OpRangeStep>.GetMethods (BindingFlags.Static ||| BindingFlags.NonPublic)
            |> Array.filter (fun ``method`` -> ``method``.Name = "op_RangeStep")
            |> Array.map (fun ``method`` -> ``method``.ReturnType.GetGenericArguments().[0], ``method``)
            |> dict

        static member TypeProvided ``type`` = methods.ContainsKey ``type``
        static member GetMethod ``type`` = methods.[``type``]

        static member private (.. ..) (lower, incr, upper) : seq<byte>    = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<sbyte>   = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<int16>   = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<uint16>  = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<int32>   = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<uint32>  = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<int64>   = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<uint64>  = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<float32> = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<float>   = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<bigint>  = { lower .. incr .. upper}
        static member private (.. ..) (lower, incr, upper) : seq<decimal> = { lower .. incr .. upper}

    let ``-> id`` = getGenericMethodInfo <@ id @>
    let ``-> |>`` = getGenericMethodInfo <@ (|>) @>
    let ``-> <|`` = getGenericMethodInfo <@ (<|) @>
    let ``-> ..`` = getGenericMethodInfo <@ (..) @>
    let ``-> .. ..`` = getGenericMethodInfo <@ (.. ..) @>

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
    | Λ ``-> ..``    (None, [|``type``|], args) when              OpRange.TypeProvided ``type`` -> optimize <| Expr.Call (OpRange.GetMethod ``type``, args)
    | Λ ``-> .. ..`` (None, [|ty1;ty2|],  args) when ty1 = ty2 && OpRangeStep.TypeProvided ty1  -> optimize <| Expr.Call (OpRangeStep.GetMethod ty1,  args)
    | Λ ``-> |>`` (None, _, [x1;x2]) -> optimize <| Expr.Application (x2, x1)
    | Λ ``-> <|`` (None, _, [x1;x2]) -> optimize <| Expr.Application (x1, x2)
    | Patterns.Application (Lambda(var, body), input) -> optimize <| Expr.Let (var, input, body)
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

    let Conv (e:#Expr, eraseEquality) =
        let e = optimize e

        let linqExpr = ConvExpr { eraseEquality = eraseEquality; varEnv = Map.empty } e

        Expression.Lambda(linqExpr, Expression.Parameter(typeof<unit>)) |> asExpression

    let CompileImpl (e: #Expr, eraseEquality) = 
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

    