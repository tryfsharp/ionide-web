open Fable.Core  
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.monaco
open Ionide.Web

[<Emit("_monaco = monaco")>]
let hack : unit = failwith "JS only"
hack

// Use localhost services when started from localhost
if LogHelpers.isLocalHost() then
    Services.registerServices "http://localhost:7101"
    FsiService.registerService "http://localhost:7102"
else
    Services.registerServices "http://tryfsharp-autocomplete.azurewebsites.net"
    FsiService.registerService "http://tryfsharp-fable-compiler.azurewebsites.net"

/// API that's exposed to the JavaScript caller 
type IEditorWrapper = 
    abstract getEditor : unit -> editor.ICodeEditor
    abstract appendCode : string -> unit
    abstract setTestCode : string * (unit -> unit) -> unit

/// Create an editor from a given #id. Optionally takes a funciton that 
/// can be used to customize the editor options if the caller wants to
let createEditor id (customize:System.Action<editor.IEditorConstructionOptions>) =
    let options = createEmpty<editor.IEditorConstructionOptions>
    options.language <- Some "fsharp"
    options.value <- Some "printfn \"Hello world\""
    if customize <> null then customize.Invoke(options)

    let dom = Browser.document.getElementById(id)
    let services = createEmpty<editor.IEditorOverrideServices>

    let ed = monaco.editor.Globals.create(dom |> unbox, options, services )
    let fsi = FsiService.createInteractiveService ed

    let md = ed.getModel()
    md.onDidChangeContent(fun k -> 
        md |> Services.parseEditor |> ignore) |> ignore

    { new IEditorWrapper with
        member x.getEditor() = ed
        member x.appendCode(code) =
            let model = ed.getModel()
            let value = model.getValue().TrimEnd()
            model.setValue(if value = "" then code else value + "\n\n" + code)
        member x.setTestCode(code, callback) = 
            fsi.SetTestCode(code, callback) }