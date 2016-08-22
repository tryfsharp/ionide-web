open Fable.Core  
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.monaco
open Ionide.Web


[<Emit("_monaco = monaco")>]
let hack : unit = failwith "JS only"
hack

[<Emit("Object.keys($0)")>]
let keys (o:obj) : string [] = failwith "JS"

[<Emit("(function() { var exports = {}; var res = eval($0); return [exports, res]; })()")>]
let eval (s:string) : obj * obj = failwith "JS"

let dom = Browser.document.getElementById("editor")

let options = createEmpty<editor.IEditorConstructionOptions>
options.value <- Some "let t = 1"
options.language <- Some "fsharp"
options.fontSize <- Some 16.
options.lineNumbers <- Some (box false)

let services = createEmpty<editor.IEditorOverrideServices>


type BabelOptions = 
  { presets : string[] }

type BabelResult = 
  { code : string }
type Babel =
  abstract transformFromAst : obj * string * BabelOptions -> BabelResult

[<Emit("Babel")>]
let babel : Babel = Unchecked.defaultof<_> 

type Message = { 
    kind : string
    message : string 
} 

type Declaration = {
    name : string
    ``type`` : string
}

type FableResult = {
    compiled : obj
    declarations : Declaration[]
    messages : Message[]
}

let counter = ref 0
let ed = monaco.editor.Globals.create(dom |> unbox, options, services )

let zones = ResizeArray<_>()
let zoneTops = System.Collections.Generic.Dictionary<float, float>()

ed.onKeyDown(fun e -> 
  if e.altKey && e.keyCode = KeyCode.Enter then
    let sel = ed.getSelection()
    incr counter
    let id = sprintf "id%d" counter.Value
    let node = Browser.document.createElement_div()
    let wrapper = Browser.document.createElement_div()
    node.appendChild(wrapper) |> ignore
    node.style.background <- "#f0f0f0"

    let zone = createEmpty<editor.IViewZone>
    let mutable zoneId = -1.
    let top = ed.getTopForPosition(sel.endLineNumber, sel.endColumn)
    ed.changeViewZones(fun accessor ->  
      for id, z in zones do
        if zoneTops.[id] > top then
          accessor.removeZone(id)

      zone.afterLineNumber <- sel.endLineNumber
      zone.heightInPx <- Some 1.0
      zone.domNode <- node
      zoneId <- accessor.addZone(zone) 
      zones.Add( (zoneId, zone) )
      zone.onDomNodeTop <- Some(System.Func<_, _>(fun t -> zoneTops.[zoneId] <- t))

      () )


    let pos = createEmpty<IPosition>
    pos.column <- 1.
    pos.lineNumber <- sel.endLineNumber + 1.0
    if ed.getModel().getLineCount() < sel.endLineNumber + 1.0 then
      ed.getModel().setValue(ed.getModel().getValue() + "\n")
    //ed.setPosition(pos)

    promise {
        let text = ed.getModel().getValueInRange(unbox sel)
        let! res = Globals.axios().post("http://tryfsharp-fable-compiler.azurewebsites.net/fable", text)
        let res = res.data |> unbox<FableResult>
        Browser.console.log("Response:", res)
        let code = babel.transformFromAst(res.compiled, text, { presets = [| "es2015" |] })

        let fc = List.map ((+) 1) []
        let consoleLog = System.Func<_, _>(fun s -> wrapper.innerHTML <- wrapper.innerHTML + s + "<br />")
        let code = code.code.Replace("\"x=>{console.log(x)}\"", "consoleLog")
        Browser.console.log("Compiled:", code)
        for d in res.declarations do
            Browser.console.log("Declaration: %s: %s", d.name, d.``type``)
        let res = eval(code)
        Browser.console.log(keys(fst res))

        consoleLog.Invoke(string (snd res))
        zone.heightInPx <- Some wrapper.clientHeight
        ed.changeViewZones(fun a -> a.layoutZone(zoneId))

        //Browser.window.alert(wrapper.clientHeight)


    } |> ignore
(*
    let contentWidget = 
      { new editor.IContentWidget with
          member x.allowEditorOverflow = false
          member x.getId() = id
          member x.getDomNode() = node
          member x.getPosition() = 
            let pos = createEmpty<editor.IContentWidgetPosition>
            let loc = createEmpty<IPosition>
            loc.column <- 1.
            loc.lineNumber <- sel.endLineNumber
            pos.position <- loc
            pos.preference <- [| editor.ContentWidgetPositionPreference.BELOW |]
            pos }

    ed.addContentWidget(contentWidget)

    *)
    ())  |> ignore

let md = ed.getModel()
md.onDidChangeContent(fun k -> md |> Services.parseEditor |> ignore) |> ignore

Services.registerServices "http://tryfsharp-autocomplete.azurewebsites.net"


let a<'T> = unbox (obj())

a + 1

|> ignore