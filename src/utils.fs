namespace Ionide.Web

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

// --------------------------------------------------------------------------------------
// Logging and tracing functions
// --------------------------------------------------------------------------------------

module LogHelpers = 
    [<Emit("console.log.apply(console, $0)")>]
    let consoleLog (args:obj[]) : unit = failwith "JS only"

    let isLocalHost() = 
      Browser.window.location.hostname = "localhost" || 
      Browser.window.location.hostname = "127.0.0.1"

    let enabledCategories = 
      if not (isLocalHost ()) then set []
      else set [ "AUTOCOMPLETE"; "FSI"; "FSIGUI" ]

open LogHelpers

type Log =
  static member message(level:string, category:string, msg:string, [<System.ParamArray>] args) = 
    let args = if args = null then [| |] else args
    let category = category.ToUpper()
    if level = "EXCEPTION" || level = "ERROR" || enabledCategories.Contains category then
      let dt = System.DateTime.Now
      let p2 (s:int) = (string s).PadLeft(2, '0')
      let p4 (s:int) = (string s).PadLeft(4, '0')
      let prefix = sprintf "[%s:%s:%s:%s] %s: " (p2 dt.Hour) (p2 dt.Minute) (p2 dt.Second) (p4 dt.Millisecond) category
      let color = 
        match level with
        | "TRACE" -> "color:#808080"
        | "EXCEPTION" -> "color:#c00000"
        | "ERROR" -> "color:#900000"
        | _ -> ""
      consoleLog(FSharp.Collections.Array.append [|box ("%c" + prefix + msg); box color|] args)

  static member trace(category:string, msg:string, [<System.ParamArray>] args) = 
    Log.message("TRACE", category, msg, args)

  static member exn(category:string, msg:string, [<System.ParamArray>] args) = 
    Log.message("EXCEPTION", category, msg, args)

  static member error(category:string, msg:string, [<System.ParamArray>] args) = 
    Log.message("ERROR", category, msg, args)

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

// --------------------------------------------------------------------------------------
// Lightweight HTML DSL
// --------------------------------------------------------------------------------------

module Html = 
    open Fable.Import.Browser

    type DomAttribute = 
      | Event of (HTMLElement -> Event -> unit)
      | Property of string

    type DomNode = 
      | Text of string
      | Html of string
      | Element of tag:string * attributes:(string * DomAttribute)[] * children : DomNode[]

    let rec render node = 
      match node with
      | Html(s) ->
          let wrapper = document.createElement_div()
          wrapper.innerHTML <- s
          wrapper :> Node

      | Text(s) -> 
          document.createTextNode(s) :> Node

      | Element(tag, attrs, children) ->
          let el = document.createElement(tag)
          for c in children do el.appendChild(render c) |> ignore
          for k, a in attrs do 
            match a with
            | Property(v) -> el.setAttribute(k, v)
            | Event(f) -> el.addEventListener(k, U2.Case1(EventListener(f el)))
          el :> Node

    let renderTo (node:HTMLElement) dom = 
        while box node.lastChild <> null do ignore(node.removeChild(node.lastChild))
        node.appendChild(render dom) |> ignore
  
    let text s = Text(s)
    let html s = Html(s)
    let (=>) k v = k, Property(v)
    let (=!>) k f = k, Event(f)

    type El() = 
      static member (?) (_:El, n:string) = fun a b ->
        Element(n, Array.ofList a, Array.ofList b)

    let h = El()
