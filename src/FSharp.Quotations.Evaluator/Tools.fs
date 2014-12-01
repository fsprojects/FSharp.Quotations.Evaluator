module internal FSharp.Quotations.Evaluator.Tools

open System.Linq.Expressions
open System
open HelperTypes
open System.Reflection
open Microsoft.FSharp.Quotations
let rec getExpressionFromTuple (tuple:Expression) idx =
    if idx >= 7
        then getExpressionFromTuple (Expression.Property(tuple, "Rest")) (idx-7)
        else Expression.Property (tuple, "Item" + (idx+1).ToString()) :> Expression

let createTuple<'a> types =
    let tuple = typedefof<'a>.MakeGenericType types
    let ``constructor`` = tuple.GetConstructor types
    let callConstructor (x:seq<Expression>) = 
        Expression.New (``constructor``, x) :> Expression
    tuple, callConstructor

let unitNull = Expression.Constant(null, typeof<unit>) :> Expression

let rec createGenericTupleType types =
    match types with
    | [||]                      -> typeof<Unit>, (fun _ -> unitNull)
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

let getFuncType (args:Type[])  = 
    if args.Length <= 17 then 
        Expression.GetFuncType args
    else
        match args.Length with 
        | 18 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
        | 19 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
        | 20 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
        | 21 -> typedefof<FuncHelper<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>.MakeGenericType args
        | _ -> raise <| NotSupportedException "Quotation expressions with statements or closures containing more then 20 free variables may not be translated in this release of the F# PowerPack. This is due to limitations in the variable binding expression forms available in LINQ expression trees"
            
type FuncFSharp<'state,'a> (func:Func<'state,'a>) =
    inherit FSharpFunc<unit, 'a>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke _ = func.Invoke this.State

type FuncFSharp<'state,'a,'b> (func:Func<'state,'a,'b>) =
    inherit FSharpFunc<'a,'b>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke a = func.Invoke (this.State,a)

type FuncFSharp<'state,'a,'b,'c> (func:Func<'state,'a,'b,'c>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c>()
    [<Core.DefaultValue false>] val mutable State : 'state
    override this.Invoke (a,b) = func.Invoke (this.State,a,b)
    member __.Function = func
    override this.Invoke a = fun b -> func.Invoke (this.State,a,b)

type FuncFSharp<'state,'a,'b,'c,'d> (func:Func<'state,'a,'b,'c,'d>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c) = func.Invoke (this.State,a,b,c)
    override this.Invoke a = fun b c -> func.Invoke (this.State,a,b,c)

type FuncFSharp<'state,'a,'b,'c,'d,'e> (func:Func<'state,'a,'b,'c,'d,'e>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d) = func.Invoke (this.State,a,b,c,d)
    override this.Invoke a = fun b c d -> func.Invoke (this.State,a,b,c,d)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f> (func:Func<'state,'a,'b,'c,'d,'e,'f>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = func.Invoke (this.State,a,b,c,d,e)
    override this.Invoke a = fun b c d e -> func.Invoke (this.State,a,b,c,d,e)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f -> func.Invoke (this.State,a,b,c,d,e,f)
    override this.Invoke a = fun b c d e f -> func.Invoke (this.State,a,b,c,d,e,f)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g -> func.Invoke (this.State,a,b,c,d,e,f,g)
    override this.Invoke a = fun b c d e f g -> func.Invoke (this.State,a,b,c,d,e,f,g)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h -> func.Invoke (this.State,a,b,c,d,e,f,g,h)
    override this.Invoke a = fun b c d e f g h -> func.Invoke (this.State,a,b,c,d,e,f,g,h)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i)
    override this.Invoke a = fun b c d e f g h i -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j)
    override this.Invoke a = fun b c d e f g h i j -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k)
    override this.Invoke a = fun b c d e f g h i j k -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l)
    override this.Invoke a = fun b c d e f g h i j k l -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m)
    override this.Invoke a = fun b c d e f g h i j k l m -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
    override this.Invoke a = fun b c d e f g h i j k l m n -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p> (func:Func<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n o -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
    override this.Invoke a = fun b c d e f g h i j k l m n o -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q> (func:FuncHelper<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p->'q>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n o p -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    override this.Invoke a = fun b c d e f g h i j k l m n o p -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r> (func:FuncHelper<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p->'q->'r>()
    [<Core.DefaultValue false>] val mutable State : 'state
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n o p q -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
    override this.Invoke a = fun b c d e f g h i j k l m n o p q -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s> (func:FuncHelper<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p->'q->'r->'s>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n o p q r -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
    override this.Invoke a = fun b c d e f g h i j k l m n o p q r -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s,'t> (func:FuncHelper<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s,'t>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p->'q->'r->'s->'t>()
    [<Core.DefaultValue false>] val mutable State : 'state
    member __.Function = func
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n o p q r s -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
    override this.Invoke a = fun b c d e f g h i j k l m n o p q r s -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

let getFuncFSharpTypedef varsCount =
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
    | 16 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
    | 17 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
    | 18 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
    | 19 -> typedefof<FuncFSharp<_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_>>
    | _ -> raise <| NotSupportedException "Linq expression trees appear unable to compile to delegate that have more generic types"

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

let ``-> getArray`` = getGenericMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.GetArray : int[] -> int -> int @>
let ``-> setArray`` = getGenericMethodInfo <@ LanguagePrimitives.IntrinsicFunctions.SetArray : int[] -> int -> int -> unit @>

let ``-> id`` = getGenericMethodInfo <@ id @>
let ``-> |>`` = getGenericMethodInfo <@ (|>) @>
let ``-> <|`` = getGenericMethodInfo <@ (<|) @>
let ``-> ..`` = getGenericMethodInfo <@ (..) @>
let ``-> .. ..`` = getGenericMethodInfo <@ (.. ..) @>
