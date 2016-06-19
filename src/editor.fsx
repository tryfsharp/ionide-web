#r "../node_modules/fable-core/Fable.Core.dll"
#load "Fable.Import.Monaco.fsx"

open Fable.Core  
open Fable.Import
open Fable.Import.monaco

[<Emit("_monaco = monaco")>]
let hack : unit = failwith "JS only"
hack

let dom = Browser.document.getElementsByClassName("container").[0]

let options = createEmpty<editor.IEditorConstructionOptions>
options.value <- Some "let t = 1"
options.language <- Some "fsharp"

let services = createEmpty<editor.IEditorOverrideServices>


let editor = monaco.editor.Globals.create(dom |> unbox, options, services )
 