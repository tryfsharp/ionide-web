namespace Ionide.Web

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

// --------------------------------------------------------------------------------------
// Computation builder for JS promises (by Dave Thomas)
// --------------------------------------------------------------------------------------

module Promise =
    open Fable.Import.JS

    let success (a : 'T -> 'R) (pr : Promise<'T>) : Promise<'R> =
        pr?``then`` $ a |> unbox 

    let bind (a : 'T -> Promise<'R>) (pr : Promise<'T>) : Promise<'R> =
        pr?``then`` $ a |> unbox

    let fail (a : obj -> 'T)  (pr : Promise<'T>) : Promise<'T> =
        pr.catch (unbox<Func<obj, U2<'T, PromiseLike<'T>>>> a)
        
    let either a  (b: Func<obj, U2<'R, JS.PromiseLike<'R>>>) (pr : Promise<'T>) : Promise<'R> =
        pr.``then``(a, b)

    let lift<'T> (a : 'T) : Promise<'T> =
        Promise.resolve(U2.Case1 a)
    
type PromiseBuilder() =
    member inline x.Bind(m,f) = Promise.bind f m
    member inline x.Return(a) = Promise.lift a
    member inline x.ReturnFrom(a) = a
    member inline x.Delay(f) = f
    member inline x.Zero() = Fable.Import.JS.Promise.resolve()
    member inline x.Combine(a, b) = Promise.bind (fun () -> b()) a 
    member inline x.Run(f) = f()
    member inline x.For(s:seq<_>, b) = 
      let e = s.GetEnumerator()
      let rec loop () = 
        if e.MoveNext() then Promise.bind (fun () -> loop()) (b e.Current)
        else x.Zero()
      loop ()


[<AutoOpen>]
module PromiseBuilderImp =
    let promise = PromiseBuilder()
