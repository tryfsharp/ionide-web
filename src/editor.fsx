#r "../node_modules/fable-core/Fable.Core.dll"
#load "Fable.Import.Axios.fsx"
#load "Fable.Import.Monaco.fsx"

open Fable.Core  
open Fable.Import
open Fable.Import.monaco
open Fable.Import.Axios

//---------------------------------------------------
//DTOs
//---------------------------------------------------
[<AutoOpen>]
module DTO =

    type ParseRequest = { FileName : string; IsAsync : bool; Lines : string[]}
    type ProjectRequest = { FileName : string}
    type DeclarationsRequest = {FileName : string}
    type HelptextRequest = {Symbol : string}
    type PositionRequest = {FileName : string; Line : int; Column : int; Filter : string}
    type CompletionRequest = {FileName : string; SourceLine : string; Line : int; Column : int; Filter : string}

    type OverloadSignature = {
        Signature: string
        Comment: string
    }

    type Error = {
        /// 1-indexed first line of the error block
        StartLine : int
        /// 1-indexed first column of the error block
        StartColumn : int
        /// 1-indexed last line of the error block
        EndLine : int
        /// 1-indexed last column of the error block
        EndColumn : int
        /// Description of the error
        Message : string
        ///Severity of the error - warning or error
        Severity : string
        /// Type of the Error
        Subcategory : string
        ///File Name
        FileName : string
        }

    type Declaration = {
        File : string
        Line : int
        Column : int
    }

    type Completion = {
        Name : string
        ReplacementText : string
        Glyph : string
        GlyphChar: string
    }

    type SymbolUse = {
        FileName : string
        StartLine : int
        StartColumn : int
        EndLine : int
        EndColumn : int
        IsFromDefinition : bool
        IsFromAttribute : bool
        IsFromComputationExpression : bool
        IsFromDispatchSlotImplementation : bool
        IsFromPattern : bool
        IsFromType : bool
    }

    type SymbolUses = {
        Name : string
        Uses : SymbolUse array
    }

    type Helptext = {
        Name : string
        Overloads: OverloadSignature [] []
    }

    type OverloadParameter = {
        Name : string
        CanonicalTypeTextForSorting : string
        Display : string
        Description : string
    }
    type Overload = {
        Tip : OverloadSignature [] []
        TypeText : string
        Parameters : OverloadParameter []
        IsStaticArguments : bool
    }

    type Method = {
        Name : string
        CurrentParameter : int
        Overloads : Overload []
    }

    type CompilerLocation = {
        Fcs : string
        Fsi : string
        MSBuild : string
    }

    type Range = {
        StartColumn: int
        StartLine: int
        EndColumn: int
        EndLine: int
    }

    type Symbol ={
        UniqueName: string
        Name: string
        Glyph: string
        GlyphChar: string
        IsTopLevel: bool
        Range: Range
        BodyRange : Range
    }

    type Symbols = {
        Declaration : Symbol;
        Nested : Symbol []
    }


    type Result<'T> = {Kind : string; Data : 'T}
    type CompilerLocationResult = Result<CompilerLocation>
    type HelptextResult = Result<Helptext>
    type CompletionResult = Result<Completion[]>
    type SymbolUseResult = Result<SymbolUses>
    type TooltipResult = Result<OverloadSignature[][]>
    type ParseResult = Result<Error[]>
    type FindDeclarationResult = Result<Declaration>
    type MethodResult = Result<Method>
    type DeclarationResult = Result<Symbols[]>

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
//PromisesExt (by Dave)
//---------------------------------------------------
module Promise =
    open System
    open Fable.Core
    open Fable.Import
    open Fable.Import.JS
    
    let success (a : 'T -> 'R) (pr : Promise<'T>) : Promise<'R> =
        pr?``then`` $ a |> unbox 

    let bind (a : 'T -> Promise<'R>) (pr : Promise<'T>) : Promise<'R> =
        pr?``then`` $ a |> unbox

    //catch Func<obj, U2<'T, JS.PromiseLike<'T>>>> -> JS.Promise<'T>
    //catch Func<obj, Unit>> -> JS.Promise<'T> 
    let fail (a : obj -> 'T)  (pr : Promise<'T>) : Promise<'T> =
        pr.catch (unbox<Func<obj, U2<'T, PromiseLike<'T>>>> a)
        
    //then Func<'T, U2<'R, JS.PromiseLike<'R>>>> * Func<obj, U2<'R, JS.PromiseLike<'R>>>> -> 'R
    //then Func<'T, U2<'R, JS.PromiseLike<'R>>>> * <Func<obj, Unit>> onrejected) -> 'R
    let either a  (b: Func<obj, U2<'R, JS.PromiseLike<'R>>>) (pr : Promise<'T>) : Promise<'R> =
        pr.``then``(a, b)
        //pr?``then`` $ (a, b) |> unbox

    let lift<'T> (a : 'T) : Promise<'T> =
        Promise.resolve(U2.Case1 a)
    
type PromiseBuilder() =
        member inline x.Bind(m,f) = Promise.bind f m
        member inline x.Return(a) = Promise.lift a
        member inline x.ReturnFrom(a) = a
        member inline x.Zero() = Fable.Import.JS.Promise.resolve()

[<AutoOpen>]
module PromiseBuilderImp =
    let promise = PromiseBuilder()

//---------------------------------------------------
//Communication
//---------------------------------------------------
let request<'a, 'b> (obj : 'a) endpoint id = 
    let hn = Browser.window.location.hostname
    let ep = sprintf "http://%s:81/%s" hn endpoint
    Globals.axios.post (ep, obj) 
    |> Promise.success(fun r -> (r.data |> unbox<string[]>).[id] |> JS.JSON.parse |> unbox<'b>)
  


let parse s = request<ParseRequest, ParseResult> s "parse" 0
let completion s = request<CompletionRequest, CompletionResult> s "completion" 1
let tooltip s = request<PositionRequest, TooltipResult> s "tooltip" 0


//---------------------------------------------------
//Features providers
//---------------------------------------------------

let hoverProvider = {
    new languages.HoverProvider
    with 
        member this.provideHover(model, position, token) = 
            promise {
                let! o = tooltip {FileName = "test.fsx"; Line = position.lineNumber |> unbox; Column = position.column |> unbox; Filter = ""}
                let res = (o.Data |> Array.fold (fun acc n -> (n |> Array.toList) @ acc ) []).Head

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

                return h
            } |> Case2
}

let parseEditor (model : editor.IModel) = promise {
    let content =  model.getValue (editor.EndOfLinePreference.TextDefined, true)
    let! res = parse {FileName = "test.fsx"; IsAsync = true; Lines = content.Split('\n')  }
    return ()
}



//---------------------------------------------------
//Create editor
//---------------------------------------------------

monaco.languages.Globals.registerHoverProvider("fsharp", hoverProvider)


let ed = monaco.editor.Globals.create(dom |> unbox, options, services )
ed.onKeyUp (fun k -> ed.getModel() |> parseEditor |> ignore)