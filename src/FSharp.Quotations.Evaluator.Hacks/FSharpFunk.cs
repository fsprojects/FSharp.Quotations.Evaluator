namespace FSharp.Quotations.Evaluator.Hacks
{
    public abstract class FSharpFunk<T, TResult> : Microsoft.FSharp.Core.FSharpFunc<T, TResult>
    {
        public abstract override TResult Invoke(T func);
    }
}