namespace FSharp.Quotations.Evaluator

[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("FSharp.Quotations.Evaluator.Tests")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleTo("FSharp.Quotations.Evaluator.NetStandard.Tests")>]
do ()

/// A set of types used for implementing quotation conversions.
/// These are public only because targets of Linq Lambda expressions require them to be so
module HelperTypes = 
    type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10,'T11,'T12,'T13,'T14,'T15,'T16,'T17,'TResult>                = delegate of 'T1*'T2*'T3*'T4*'T5*'T6*'T7*'T8*'T9*'T10*'T11*'T12*'T13*'T14*'T15*'T16*'T17                  -> 'TResult 
    type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10,'T11,'T12,'T13,'T14,'T15,'T16,'T17,'T18,'TResult>           = delegate of 'T1*'T2*'T3*'T4*'T5*'T6*'T7*'T8*'T9*'T10*'T11*'T12*'T13*'T14*'T15*'T16*'T17*'T18             -> 'TResult 
    type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10,'T11,'T12,'T13,'T14,'T15,'T16,'T17,'T18,'T19,'TResult>      = delegate of 'T1*'T2*'T3*'T4*'T5*'T6*'T7*'T8*'T9*'T10*'T11*'T12*'T13*'T14*'T15*'T16*'T17*'T18*'T19        -> 'TResult 
    type FuncHelper<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9,'T10,'T11,'T12,'T13,'T14,'T15,'T16,'T17,'T18,'T19,'T20,'TResult> = delegate of 'T1*'T2*'T3*'T4*'T5*'T6*'T7*'T8*'T9*'T10*'T11*'T12*'T13*'T14*'T15*'T16*'T17*'T18*'T19*'T20   -> 'TResult 
