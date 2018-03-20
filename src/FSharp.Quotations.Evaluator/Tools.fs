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

#if PORTABLE
[<AutoOpen>]
module ReflectionAdapters = 

    [<Flags>]
    type BindingFlags =
        | DeclaredOnly = 2
        | Instance = 4 
        | Static = 8
        | Public = 16
        | NonPublic = 32
    let inline hasFlag (flag : BindingFlags) f  = (f &&& flag) = flag
    let isDeclaredFlag  f    = hasFlag BindingFlags.DeclaredOnly f
    let isPublicFlag    f    = hasFlag BindingFlags.Public f
    let isStaticFlag    f    = hasFlag BindingFlags.Static f
    let isInstanceFlag  f    = hasFlag BindingFlags.Instance f
    let isNonPublicFlag f    = hasFlag BindingFlags.NonPublic f

    [<System.Flags>]
    type TypeCode = 
        | Int32     = 0
        | Int64     = 1
        | Byte      = 2
        | SByte     = 3
        | Int16     = 4
        | UInt16    = 5
        | UInt32    = 6
        | UInt64    = 7
        | Single    = 8
        | Double    = 9
        | Decimal   = 10
        | Other     = 11
        
    let isAcceptable bindingFlags isStatic isPublic =
        // 1. check if member kind (static\instance) was specified in flags
        ((isStaticFlag bindingFlags && isStatic) || (isInstanceFlag bindingFlags && not isStatic)) && 
        // 2. check if member accessibility was specified in flags
        ((isPublicFlag bindingFlags && isPublic) || (isNonPublicFlag bindingFlags && not isPublic))
    
    let publicFlags = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static

    let commit (results : _[]) = 
        match results with
        | [||] -> null
        | [| m |] -> m
        | _ -> raise (AmbiguousMatchException())

    let canUseAccessor (accessor : MethodInfo) nonPublic = 
        box accessor <> null && (accessor.IsPublic || nonPublic)
    
    open PrimReflectionAdapters

    type System.Type with        
        member inline this.IsGenericType = this.GetTypeInfo().IsGenericType
        member this.GetNestedType (name, bindingFlags) = 
            // MSDN: http://msdn.microsoft.com/en-us/library/0dcb3ad5.aspx
            // The following BindingFlags filter flags can be used to define which nested types to include in the search:
            // You must specify either BindingFlags.Public or BindingFlags.NonPublic to get a return.
            // Specify BindingFlags.Public to include public nested types in the search.
            // Specify BindingFlags.NonPublic to include non-public nested types (that is, private, internal, and protected nested types) in the search.
            // This method returns only the nested types of the current type. It does not search the base classes of the current type. 
            // To find types that are nested in base classes, you must walk the inheritance hierarchy, calling GetNestedType at each level.
            let nestedTyOpt =                
                this.GetTypeInfo().DeclaredNestedTypes
                |> Seq.tryFind (fun nestedTy -> 
                    nestedTy.Name = name && (
                        (isPublicFlag bindingFlags && nestedTy.IsNestedPublic) || 
                        (isNonPublicFlag bindingFlags && (nestedTy.IsNestedPrivate || nestedTy.IsNestedFamily || nestedTy.IsNestedAssembly || nestedTy.IsNestedFamORAssem || nestedTy.IsNestedFamANDAssem))
                        )
                    )
                |> Option.map (fun ti -> ti.AsType())
            defaultArg nestedTyOpt null
        // use different sources based on Declared flag
        member this.GetMethods(bindingFlags) = 
            (if isDeclaredFlag bindingFlags then this.GetTypeInfo().DeclaredMethods else this.GetRuntimeMethods())
            |> Seq.filter (fun m -> isAcceptable bindingFlags m.IsStatic m.IsPublic)
            |> Seq.toArray
        // use different sources based on Declared flag
        member this.GetFields(bindingFlags) = 
            (if isDeclaredFlag bindingFlags then this.GetTypeInfo().DeclaredFields else this.GetRuntimeFields())
            |> Seq.filter (fun f -> isAcceptable bindingFlags f.IsStatic f.IsPublic)
            |> Seq.toArray
        // use different sources based on Declared flag
        member this.GetProperties(?bindingFlags) = 
            let bindingFlags = defaultArg bindingFlags publicFlags
            (if isDeclaredFlag bindingFlags then this.GetTypeInfo().DeclaredProperties else this.GetRuntimeProperties())
            |> Seq.filter (fun pi-> 
                let mi = if pi.GetMethod <> null then pi.GetMethod else pi.SetMethod
                assert (mi <> null)
                isAcceptable bindingFlags mi.IsStatic mi.IsPublic
                )
            |> Seq.toArray
        // use different sources based on Declared flag
        member this.GetMethod(name, ?bindingFlags) =
            let bindingFlags = defaultArg bindingFlags publicFlags
            this.GetMethods(bindingFlags)
            |> Array.filter(fun m -> m.Name = name)
            |> commit
        member inline this.GetMethod(name, parameterTypes) = this.GetRuntimeMethod(name, parameterTypes)
    // use different sources based on Declared flag
        member this.GetProperty(name, bindingFlags) = 
            this.GetProperties(bindingFlags)
            |> Array.filter (fun pi -> pi.Name = name)
            |> commit
        member this.IsGenericTypeDefinition = this.GetTypeInfo().IsGenericTypeDefinition
        member this.GetGenericArguments() = 
            if this.IsGenericTypeDefinition then this.GetTypeInfo().GenericTypeParameters
            elif this.IsGenericType then this.GenericTypeArguments
            else [||]
        member this.BaseType = this.GetTypeInfo().BaseType
        member this.GetConstructor(parameterTypes : Type[]) = 
            this.GetTypeInfo().DeclaredConstructors
            |> Seq.filter (fun ci ->
                not ci.IsStatic && //exclude type initializer
                (
                    let parameters = ci.GetParameters()
                    (parameters.Length = parameterTypes.Length) &&
                    (parameterTypes, parameters) ||> Array.forall2 (fun ty pi -> pi.ParameterType.Equals ty) 
                )
            )
            |> Seq.toArray
            |> commit
        // MSDN: returns an array of Type objects representing all the interfaces implemented or inherited by the current Type.
        member this.GetInterfaces() = this.GetTypeInfo().ImplementedInterfaces |> Seq.toArray
        member this.GetConstructors(?bindingFlags) = 
            let bindingFlags = defaultArg bindingFlags publicFlags
            // type initializer will also be included in resultset
            this.GetTypeInfo().DeclaredConstructors 
            |> Seq.filter (fun ci -> isAcceptable bindingFlags ci.IsStatic ci.IsPublic)
            |> Seq.toArray
        member this.GetMethods() = this.GetMethods(publicFlags)
        member this.Assembly = this.GetTypeInfo().Assembly
        member this.IsSubclassOf(otherTy : Type) = this.GetTypeInfo().IsSubclassOf(otherTy)
        member this.IsEnum = this.GetTypeInfo().IsEnum;
        member this.GetField(name, bindingFlags) = 
            this.GetFields(bindingFlags)
            |> Array.filter (fun fi -> fi.Name = name)
            |> commit
        member this.GetProperty(name, propertyType, parameterTypes : Type[]) = 
            this.GetProperties()
            |> Array.filter (fun pi ->
                pi.Name = name &&
                pi.PropertyType = propertyType &&
                (
                    let parameters = pi.GetIndexParameters()
                    (parameters.Length = parameterTypes.Length) &&
                    (parameterTypes, parameters) ||> Array.forall2 (fun ty pi -> pi.ParameterType.Equals ty)
                )
            )
            |> commit
        static member GetTypeCode(ty : Type) = 
            if   typeof<System.Int32>.Equals ty  then TypeCode.Int32
            elif typeof<System.Int64>.Equals ty  then TypeCode.Int64
            elif typeof<System.Byte>.Equals ty   then TypeCode.Byte
            elif ty = typeof<System.SByte>  then TypeCode.SByte
            elif ty = typeof<System.Int16>  then TypeCode.Int16
            elif ty = typeof<System.UInt16> then TypeCode.UInt16
            elif ty = typeof<System.UInt32> then TypeCode.UInt32
            elif ty = typeof<System.UInt64> then TypeCode.UInt64
            elif ty = typeof<System.Single> then TypeCode.Single
            elif ty = typeof<System.Double> then TypeCode.Double
            elif ty = typeof<System.Decimal> then TypeCode.Decimal
            else TypeCode.Other

    type System.Reflection.MemberInfo with
        member this.GetCustomAttributes(attrTy, inherits) : obj[] = downcast box(CustomAttributeExtensions.GetCustomAttributes(this, attrTy, inherits) |> Seq.toArray)

    type System.Reflection.MethodInfo with
        member this.GetCustomAttributes(inherits : bool) : obj[] = downcast box(CustomAttributeExtensions.GetCustomAttributes(this, inherits) |> Seq.toArray)

    type System.Reflection.PropertyInfo with
        member this.GetGetMethod(nonPublic) = 
            let mi = this.GetMethod
            if canUseAccessor mi nonPublic then mi 
            else null
        member this.GetSetMethod(nonPublic) = 
            let mi = this.SetMethod
            if canUseAccessor mi nonPublic then mi
            else null
    
    type System.Reflection.Assembly with
        member this.GetTypes() = 
            this.DefinedTypes 
            |> Seq.map (fun ti -> ti.AsType())
            |> Seq.toArray

    type System.Delegate with
        static member CreateDelegate(delegateType, methodInfo : MethodInfo) = methodInfo.CreateDelegate(delegateType)
        static member CreateDelegate(delegateType, obj : obj, methodInfo : MethodInfo) = methodInfo.CreateDelegate(delegateType, obj)            
#endif

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
            
type FuncFSharp<'state,'a> (f:Func<'state,'a>) =
    inherit Hacks.FSharpFunk<unit, 'a>()
    [<Core.DefaultValue false>] val mutable State : 'state
    override this.Invoke _ = f.Invoke this.State

type FuncFSharp<'state,'a,'b> (f:Func<'state,'a,'b>) =
    inherit Hacks.FSharpFunk<'a,'b>()
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

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q> (func:FuncHelper<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p->'q>()
    [<Core.DefaultValue false>] val mutable State : 'state
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
    override this.Invoke (a,b,c,d,e) = fun f g h i j k l m n o p q r -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
    override this.Invoke a = fun b c d e f g h i j k l m n o p q r -> func.Invoke (this.State,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

type FuncFSharp<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s,'t> (func:FuncHelper<'state,'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s,'t>) =
    inherit OptimizedClosures.FSharpFunc<'a,'b,'c,'d,'e,'f->'g->'h->'i->'j->'k->'l->'m->'n->'o->'p->'q->'r->'s->'t>()
    [<Core.DefaultValue false>] val mutable State : 'state
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
let ``-> checked.int``    = getGenericMethodInfo <@ Checked.int @>
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

let private languagePrimitivesType =
    typedefof<_ list>.Assembly.GetType("Microsoft.FSharp.Core.LanguagePrimitives")

let private parseInt32M = languagePrimitivesType.GetMethod("ParseInt32")
let private parseInt64M = languagePrimitivesType.GetMethod("ParseInt64")
let private parseUInt32M = languagePrimitivesType.GetMethod("ParseUInt32")
let private parseUInt64M = languagePrimitivesType.GetMethod("ParseUInt64")

let parseInt32Expr argsP = Expression.Call(null, parseInt32M, argsP)
let parseInt64Expr argsP = Expression.Call(null, parseInt64M, argsP)
let parseUInt32Expr argsP = Expression.Call(null, parseUInt32M, argsP)
let parseUInt64Expr argsP = Expression.Call(null, parseUInt64M, argsP)

let parseSByteExpr argsP = Expression.Convert(Expression.Call(null, parseInt32M, argsP), typeof<sbyte>)
let parseInt16Expr argsP = Expression.Convert(Expression.Call(null, parseInt32M, argsP), typeof<int16>)
let parseByteExpr argsP = Expression.Convert(Expression.Call(null, parseUInt32M, argsP), typeof<byte>)
let parseUInt16Expr argsP = Expression.Convert(Expression.Call(null, parseUInt32M, argsP), typeof<uint16>)

let private parseSingleM = typeof<float32>.GetMethod("Parse", [|typeof<string>; typeof<Globalization.NumberStyles>; typeof<IFormatProvider>|])
let private parseDoubleM = typeof<float>.GetMethod("Parse", [|typeof<string>; typeof<Globalization.NumberStyles>; typeof<IFormatProvider>|])
let private parseDecimalM = typeof<decimal>.GetMethod("Parse", [|typeof<string>; typeof<Globalization.NumberStyles>; typeof<IFormatProvider>|])

let private numStyleConst = Expression.Constant(Globalization.NumberStyles.Float) :> Expression
let private providerConst = Expression.Constant(Globalization.CultureInfo.InvariantCulture) :> Expression

let parseSingleExpr (argsExprs: Expression[]) =
    let argsExprs =
        [| yield! argsExprs
           yield numStyleConst
           yield providerConst |]
    Expression.Call(null, parseSingleM, argsExprs)
let parseDoubleExpr (argsExprs: Expression[]) =
    let argsExprs =
        [| yield! argsExprs
           yield numStyleConst
           yield providerConst |]
    Expression.Call(null, parseDoubleM, argsExprs)
let parseDecimalExpr (argsExprs: Expression[]) =
    let argsExprs =
        [| yield! argsExprs
           yield numStyleConst
           yield providerConst |]
    Expression.Call(null, parseDecimalM, argsExprs)

let private parseCharM = typeof<char>.GetMethod("Parse")

let parseCharExpr argsP = Expression.Call(null, parseCharM, argsP)