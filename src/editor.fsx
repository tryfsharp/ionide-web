#r "../node_modules/fable-core/Fable.Core.dll"
#load "Fable.Import.Monaco.fsx"

open Fable.Core  
open Fable.Import
open Fable.Import.monaco

//---------------------------------------------------
//Initialization
//---------------------------------------------------

[<Emit("_monaco = monaco")>]
let hack : unit = failwith "JS only"
hack

let dom = Browser.document.getElementsByClassName("container").[0]

let options = createEmpty<editor.IEditorConstructionOptions>
options.value <- Some "let t = 1"
options.language <- Some "fsharp"

let services = createEmpty<editor.IEditorOverrideServices>


//---------------------------------------------------
//Features providers
//---------------------------------------------------

let hoverProvider = {
    new languages.HoverProvider
    with 
        member this.provideHover(model, position, token) = 
            let h = createEmpty<languages.Hover>
            let ctn = createEmpty<IHTMLContentElement>
            ctn.markdown <- Some "aaa"

            let range = createEmpty<IRange>
            range.startLineNumber <- position.lineNumber
            range.endLineNumber <- position.lineNumber
            range.startColumn <- position.column
            range.endColumn <- position.column + 1.


            h.htmlContent <- ResizeArray([ctn])
            h.range <- range

            Case1 h

}



//---------------------------------------------------
//Create editor
//---------------------------------------------------

monaco.languages.Globals.registerHoverProvider("fsharp", hoverProvider)


let editor = monaco.editor.Globals.create(dom |> unbox, options, services )
