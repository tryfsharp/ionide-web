module Ionide.Web.Services

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Ionide.Web
open Fable.Import.monaco

// --------------------------------------------------------------------------------------
// Comunication 
// --------------------------------------------------------------------------------------

module Communication = 
    let mutable root = ""
    let request<'a, 'b> (obj : 'a) endpoint id = 
        if root = "" then failwith "Root URL for services not set!"
        let ep = sprintf "%s/%s" root endpoint
        Globals.axios().post (ep, obj) 
        |> Promise.success(fun r -> (r.data |> unbox<string[]>).[id] |> JS.JSON.parse |> unbox<'b>)
 
    let parse s = request<ParseRequest, ParseResult> s "parse" 0
    let completion s = request<CompletionRequest, CompletionResult> s "completion" 1
    let tooltip s = request<PositionRequest, TooltipResult> s "tooltip" 0
    let helptext s = request<HelptextRequest, HelptextResult> s "helptext" 0
    let methods s = request<PositionRequest, MethodResult> s "methods" 0 
    let symbolUse s = request<PositionRequest, SymbolUseResult> s "symboluse" 0
    let declaration s = request<PositionRequest, FindDeclarationResult> s "finddeclaration" 0

// --------------------------------------------------------------------------------------
// Providers for the variouus Monaco services
// --------------------------------------------------------------------------------------

open Communication

[<Emit("_monaco = monaco")>]
let hack : unit = failwith "JS only"
hack

let hoverProvider = 
  { new languages.HoverProvider with 
        member this.provideHover(model, position, token) = Case2 <| promise {
            let! o = 
              { PositionRequest.FileName = "test.fsx"; Line = position.lineNumber |> unbox;
                Column = position.column |> unbox; Filter = "" } |> tooltip
            let res = 
                // REVIEW: Is this just: o.Data |> Seq.concat id |> Seq.head ?
                o.Data 
                |> Array.fold (fun acc n -> (n |> Array.toList) @ acc ) []
                |> List.head

            let h = createEmpty<languages.Hover>
            let ctn = createEmpty<IHTMLContentElement>
            ctn.markdown <- Some res.Signature
                            
            let w = model.getWordAtPosition(position |> unbox)

            let range = createEmpty<IRange>
            range.startLineNumber <- position.lineNumber
            range.endLineNumber <- position.lineNumber
            range.startColumn <- w.startColumn
            range.endColumn <- w.endColumn

            h.htmlContent <- ResizeArray([ctn])
            h.range <- range
            return h } }

let convertToInt code =
    match code with
    | "C" -> 6      // CompletionItemKind.Class      
    | "E" -> 12     // CompletionItemKind.Enum       
    | "S" -> 6      // CompletionItemKind.Value      
    | "I" -> 7      // CompletionItemKind.Interface  
    | "N" -> 8      // CompletionItemKind.Module     
    | "M" -> 1      // CompletionItemKind.Method     
    | "P" -> 9      // CompletionItemKind.Property   
    | "F" -> 4      // CompletionItemKind.Field      
    | "T" -> 6      // CompletionItemKind.Class      
    | _   -> 0

let completionProvider = 
  { new languages.CompletionItemProvider with 
        member this.provideCompletionItems(model, position, token) = U4.Case2 <| promise {
            let! o = 
              { CompletionRequest.FileName = "test.fsx"
                SourceLine = model.getLineContent position.lineNumber
                Line = position.lineNumber |> unbox
                Column = position.column |> unbox; 
                Filter = "StartsWith" } |> completion 
            return
                o.Data 
                |> Array.map (fun d -> 
                    let ci = createEmpty<languages.CompletionItem>
                    ci.kind <- d.GlyphChar |> convertToInt |> unbox
                    ci.label <- d.Name
                    ci.insertText <- Some d.ReplacementText
                    ci )
                |> ResizeArray }

        member this.resolveCompletionItem(item, token) = Case2 <| promise {
            let! o = helptext {Symbol = item.label }
            let res = (o.Data.Overloads |> Array.fold (fun acc n -> (n |> Array.toList) @ acc ) []).Head
            item.documentation <- Some res.Comment
            item.detail <- Some res.Signature
            return item }

        member this.triggerCharacters 
          with get () = ResizeArray(["."]) |> Some }

let parseEditor (model : editor.IModel) = promise {
    let content =  model.getValue (editor.EndOfLinePreference.TextDefined, true)
    let! res = parse {FileName = "test.fsx"; IsAsync = true; Lines = content.Split('\n')  }
    return () }

let signatureProvider = 
  { new languages.SignatureHelpProvider with
        member this.signatureHelpTriggerCharacters 
          with get () = ResizeArray(["("; ","])

        member this.provideSignatureHelp(model, position, token) = Case2 <| promise {
            let! o = 
              { PositionRequest.FileName = "test.fsx"
                Line = position.lineNumber |> unbox
                Column = position.column |> unbox
                Filter = "" } |> methods 
            let res = createEmpty<languages.SignatureHelp>
            let sigs =  o.Data.Overloads |> Array.map (fun c ->
                let tip = c.Tip.[0].[0]
                let si = createEmpty<languages.SignatureInformation>
                si.label <- tip.Signature
                si.documentation <- tip.Comment
                si.parameters <- ResizeArray ()
                for p in c.Parameters do
                    let pi = createEmpty<languages.ParameterInformation>
                    pi.label <- p.Name
                    pi.documentation <- p.CanonicalTypeTextForSorting
                    si.parameters.Add(pi)
                si )

            let sigIndex = 
                sigs 
                |> Array.sortBy (fun n -> n.parameters.Count) 
                |> Array.findIndex (fun s -> s.parameters.Count >= o.Data.CurrentParameter) 

            res.activeParameter <- float (o.Data.CurrentParameter)
            res.activeSignature <- float (sigIndex + 1)
            res.signatures <- ResizeArray sigs
            return res } }
 
let highlighterProvider = 
  { new languages.DocumentHighlightProvider with 
        member this.provideDocumentHighlights(model, position, token) = Case2 <| promise {
            let! o = 
              { PositionRequest.FileName = "test.fsx"
                Line = position.lineNumber |> unbox
                Column = position.column |> unbox
                Filter = ""} |> symbolUse 
            return
                o.Data.Uses 
                |> Array.map (fun d ->
                    let res = createEmpty<languages.DocumentHighlight> 
                    res.range <- Range(float d.StartLine, float d.StartColumn, float d.EndLine, float d.EndColumn) |> unbox
                    res.kind <- (0 |> unbox)
                    res )                    
                |> ResizeArray } }

let renameProvider = 
  { new languages.RenameProvider with
        member this.provideRenameEdits(model, position, newName, token) = Case2 <| promise {
            let! o = 
              { PositionRequest.FileName = "test.fsx"
                Line = position.lineNumber |> unbox
                Column = position.column |> unbox
                Filter = "" } |> symbolUse 
            let we = createEmpty<languages.WorkspaceEdit>
            we.edits <-
                o.Data.Uses 
                |> Array.map (fun s ->
                    let range = 
                      Range(float s.StartLine, (float s.EndColumn) - (float o.Data.Name.Length), 
                            float s.EndLine, float s.EndColumn)
                    let re = createEmpty<languages.IResourceEdit>
                    re.newText <- newName
                    re.resource <- model.uri
                    re.range <- range |> unbox
                    re) |> ResizeArray
            return we } }

let definitionProvider = 
  { new languages.DefinitionProvider with
        member this.provideDefinition(model, position, token) = Case2 <| promise {
            let! o = 
              { PositionRequest.FileName = "test.fsx" 
                Line = position.lineNumber |> unbox
                Column = position.column |> unbox
                Filter = "" } |> declaration 
            let d = createEmpty<languages.Location>
            d.range <- Range(float o.Data.Line, float o.Data.Column, float o.Data.Line, float o.Data.Column) |> unbox
            d.uri <- model.uri
            return Case1 d } }

let referenceProvider = 
  { new languages.ReferenceProvider with 
        member this.provideReferences(model, position, ctx, token) = Case2 <| promise {
            let! o = 
              { PositionRequest.FileName = "test.fsx"
                Line = position.lineNumber |> unbox; 
                Column = position.column |> unbox; 
                Filter = "" } |> symbolUse 
            return
                o.Data.Uses 
                |> Array.map (fun d ->
                    let res = createEmpty<languages.Location> 
                    res.range <- 
                      Range(float d.StartLine - 1., float d.StartColumn, 
                            float d.EndLine - 1., float d.EndColumn) |> unbox
                    res )                    
                |> ResizeArray } }

let registerServices root = 
    Communication.root <- root
    monaco.languages.Globals.registerHoverProvider("fsharp", hoverProvider) |> ignore
    monaco.languages.Globals.registerCompletionItemProvider("fsharp", completionProvider) |> ignore
    monaco.languages.Globals.registerSignatureHelpProvider("fsharp", signatureProvider) |> ignore
    monaco.languages.Globals.registerDocumentHighlightProvider("fsharp", highlighterProvider) |> ignore
    monaco.languages.Globals.registerRenameProvider("fsharp", renameProvider) |> ignore
    monaco.languages.Globals.registerDefinitionProvider("fsharp", definitionProvider) |> ignore
    monaco.languages.Globals.registerReferenceProvider("fsharp", referenceProvider) |> ignore
