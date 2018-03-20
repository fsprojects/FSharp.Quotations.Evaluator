namespace FSharp.Quotations.Evaluator.Hacks
{
    /// <summary>
    /// 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <typeparam name="TResult"></typeparam>
    public abstract class FSharpFunk<T, TResult> : Microsoft.FSharp.Core.FSharpFunc<T, TResult>
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="func"></param>
        /// <returns></returns>
        public abstract override TResult Invoke(T func);
    }
}