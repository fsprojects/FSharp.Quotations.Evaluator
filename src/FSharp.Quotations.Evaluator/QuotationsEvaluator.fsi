// (c) Microsoft Corporation 2005-2009.

namespace FSharp.Quotations.Evaluator

    open System
    open System.Linq.Expressions

    module ExtraHashCompare =
        /// An intrinsic for compiling <c>&lt;@ x <> y @&gt;</c> to expression trees
        val GenericNotEqualIntrinsic : 'T -> 'T -> bool

    [<Sealed>]
    type QuotationEvaluator = 

        /// Convert the quotation expression to LINQ expression trees
        ///
        /// This operation will only succeed for a subset of quotation expressions.
        ///
        /// Exceptions: InvalidArgumentException will be raised if the input expression is
        /// not in the subset that can be converted to a LINQ expression tree
        static member ToLinqExpression : FSharp.Quotations.Expr -> System.Linq.Expressions.Expression

        /// Compile the quotation expression by first converting to LINQ expression trees
        /// The expression is currently always compiled.
        ///
        /// Exceptions: InvalidArgumentException will be raised if the input expression is
        /// not in the subset that can be converted to a LINQ expression tree
        static member CompileUntyped : FSharp.Quotations.Expr -> obj

        /// Compile the quotation expression by first converting to LINQ expression trees
        ///
        /// Exceptions: InvalidArgumentException will be raised if the input expression is
        /// not in the subset that can be converted to a LINQ expression tree
        static member EvaluateUntyped : FSharp.Quotations.Expr -> obj

        static member internal EvaluateUntypedUsingQueryApproximations : FSharp.Quotations.Expr -> obj
    
        /// Compile the quotation expression by first converting to LINQ expression trees
        /// The expression is currently always compiled.
        ///
        /// Exceptions: InvalidArgumentException will be raised if the input expression is
        /// not in the subset that can be converted to a LINQ expression tree
        [<Obsolete("Use Evaluate")>]
        static member Compile : FSharp.Quotations.Expr<'T> -> 'T

        /// Evaluate the quotation expression by first converting to LINQ expression trees
        ///
        /// Exceptions: InvalidArgumentException will be raised if the input expression is
        /// not in the subset that can be converted to a LINQ expression tree
        static member Evaluate : FSharp.Quotations.Expr<'T> -> 'T
        
    /// This module provides Compile and Eval extension members
    /// for F# quotation values, implemented by translating to LINQ
    /// expression trees and using the LINQ dynamic compiler.
    [<AutoOpen>]
    module QuotationEvaluationExtensions =

        type FSharp.Quotations.Expr with 
              /// Convert the quotation expression to a LINQ expression tree.
              ///
              /// Exceptions: InvalidArgumentException will be raised if the input expression is
              /// not in the subset that can be converted to a LINQ expression tree
              member ToLinqExpressionUntyped : unit -> Expression

        type FSharp.Quotations.Expr<'T> with 

              /// Compile and evaluate the quotation expression by first converting to LINQ expression trees.
              /// The expression is currently always compiled.
              ///
              /// Exceptions: InvalidArgumentException will be raised if the input expression is
              /// not in the subset that can be converted to a LINQ expression tree
              member Compile : unit -> 'T

              /// Evaluate the quotation expression by first converting to LINQ expression trees.
              /// The expression is currently always compiled.
              ///
              /// Exceptions: InvalidArgumentException will be raised if the input expression is
              /// not in the subset that can be converted to a LINQ expression tree
              member Evaluate : unit -> 'T

        type FSharp.Quotations.Expr with 

              /// Compile and evaluate the quotation expression by first converting to LINQ expression trees.
              ///
              /// Exceptions: InvalidArgumentException will be raised if the input expression is
              /// not in the subset that can be converted to a LINQ expression tree
              member CompileUntyped : unit -> obj

              /// Evaluate the quotation expression by first converting to LINQ expression trees.
              /// The expression is currently always compiled.
              ///
              /// Exceptions: InvalidArgumentException will be raised if the input expression is
              /// not in the subset that can be converted to a LINQ expression tree
              member EvaluateUntyped : unit -> obj

    module QuotationEvaluationTypes =
        /// This function should not be called directly. 
        //
        // NOTE: when an F# expression tree is converted to a Linq expression tree using ToLinqExpression 
        // the transformation of <c>LinqExpressionHelper(e)</c> is simple the same as the transformation of
        // 'e'. This allows LinqExpressionHelper to be used as a marker to satisfy the C# design where 
        // certain expression trees are constructed using methods with a signature that expects an
        // expression tree of type <c>Expression<T></c> but are passed an expression tree of type T.
        val LinqExpressionHelper : 'T -> Expression<'T>
