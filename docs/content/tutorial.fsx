(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../src/FSharp.Quotations.Evaluator/bin/Release/netstandard2.0/"

(**
F# Quotations Evaluator Tutorial
========================

To reference the library:

*)
#r "FSharp.Quotations.Evaluator.dll"
open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations

(**
Evaluation
----------

To evaluate a strongly typed quotation:
*)

QuotationEvaluator.Evaluate <@ 1 + 1 @>  // 2

(**
To evaluate a weakly typed quotation, use `EvaluateUntyped`. An object is returned, which
you can cast to the expected type when it is known.
*)

let evalTuple n = 
    let v = Expr.NewTuple (List.replicate n (Expr.Value n)) // codegen (fun x -> (x,x))
    v.EvaluateUntyped() 

evalTuple 4 // obj = (4,4,4,4)
evalTuple 5 // obj = (5,5,5,5,5)


(**
Compilation
-----------

All evaluation is currently done via compilation. In the future some evalaution may be done via interpretation.
To force compilation, use "Compile".  

*)

let addPlusOne = QuotationEvaluator.Evaluate <@ fun x y -> x + y + 1 @> 

let nine = addPlusOne 3 5 // 9

(**
Extension methods
-----------------

Extension methods are available for compilation and evaluation:

*)

let onePlusOneExpression = <@ 1 + 1 @>  //  2

onePlusOneExpression.Evaluate()

(**
On-the-fly code generation
--------------------------

You can generate lambdas and compile them dynamically:
*)


let tupler = 
    let v = Var("x",typeof<int>)
    let v = Expr.Lambda(v, Expr.NewTuple [Expr.Var v; Expr.Var v]) // codegen (fun x -> (x,x))
    v.CompileUntyped() :?> (int -> (int * int))

tupler 78  // (78, 78)

(**
In this example, we generate a lambda (fun x -> (x,...,x)) for tuple size N:
*)

let invokeFunctionDynamic (f: obj) (x:obj) = 
    // Invoke an F# function dynamically
    f.GetType().InvokeMember("Invoke",System.Reflection.BindingFlags.InvokeMethod,null,f,[| box x |])

let makeTupler n = 
    let xVar = Var("x",typeof<int>)
    let lambdaExpr = Expr.Lambda(xVar, Expr.NewTuple (List.replicate n (Expr.Var xVar))) // codegen (fun x -> (x,...,x))
    let compiledLambda = lambdaExpr.CompileUntyped() 
    fun (v:int) -> 
        // Invoke the function dynamically
        invokeFunctionDynamic compiledLambda v

// tupler4 and tupler7 are compiled functions which can be invoked with different arguments
let tupler4 = makeTupler 4
let tupler7 = makeTupler 7

tupler4 76 // (76, 76, 76, 76)
tupler4 63 // (63, 63, 63, 63)
tupler7 65 // (65, 65, 65, 65, 65, 65, 65)


(**
Convert to LINQ expressions
--------------------------
*)

let onePlusTwoExpression = <@ 1 + 2 @>  //  2

let expressionTree = onePlusTwoExpression.ToLinqExpressionUntyped() // an expression tree

expressionTree.ToString() // "(1 + 2)"


