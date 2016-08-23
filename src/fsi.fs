module Ionide.Web.FsiService

open Fable.Core  
open Fable.Import
open Fable.Import.monaco
open Ionide.Web
open Ionide.Web.Html

// --------------------------------------------------------------------------------------
// Dirty JavaScript hacks and other interop
// --------------------------------------------------------------------------------------

// Minimal mapping for calling standalone Babel
type BabelOptions = 
  { presets : string[] 
    plugins : obj[] }

type BabelResult = 
  { code : string }

type Babel =
  abstract transformFromAst : obj * string * BabelOptions -> BabelResult

module JsInterop =
    /// Babel plugins as required by Fable - the JavaScript code blow is copy & paster from:
    /// https://github.com/fable-compiler/Fable/blob/master/src/fable/Fable.Client.Node/js/fable.js#L71
    ///
    /// Also note that the call to `Babel.template` in code below works because we're using
    /// babel-standalone with an extension that exposes the templating API, see:
    /// https://github.com/tryfsharp/babel-standalone
    ///
    [<Emit("""(function() { 
    // Custom plugin to remove `null;` statements (e.g. at the end of constructors)
    var removeNullStatements = {
      visitor: {
        ExpressionStatement: function(path) {
          if (path.node.expression.type == "NullLiteral")
            path.remove();
        }
      }
    };

    // Custom plugin to simulate macro expressions
    var transformMacroExpressions = {
      visitor: {
        StringLiteral: function(path) {
          if (!path.node.macro)
              return;

          try {
            var buildArgs = {}, args = path.node.args;
            for (var i = 0; i < args.length; i++) {
                buildArgs["$" + i] = args[i];
            }

            var tmp = path.node.value
                // Replace spread aguments like in `$0($1...)`
                .replace(/\$(\d+)\.\.\./, function (m, i) {
                    var rep = [], j = parseInt(i);
                    for (; j < args.length; j++) {
                        rep.push("$" + j);
                    }
                    return rep.join(",");
                })
                // Replace conditional arguments like in `/$0/g{{$1?i:}}{{$2?m:}}`
                .replace(/\{\{\$(\d+)\?(.*?)\:(.*?)\}\}/g, function (_, g1, g2, g3) {
                    var i = parseInt(g1);
                    return i < args.length && args[i].value ? g2 : g3;
                })
                // Replace optional arguments like in `$0[$1]{{=$2}}`
                .replace(/\{\{([^\}]*\$(\d+).*?)\}\}/g, function (_, g1, g2) {
                    var i = parseInt(g2);
                    return i < args.length ? g1 : "";
                });

            var buildMacro = Babel.template(tmp);
            path.replaceWithMultiple(buildMacro(buildArgs));
          }
          catch (err) {
              console.log("BABEL ERROR: Failed to parse macro: " + path.node.value);
              console.log(err.message);
              if (opts.verbose && err.stack) {
                console.log(err.stack);
              }
              process.exit(1);
          }
        }
      }
    };

    return [ transformMacroExpressions, removeNullStatements ]; })()""")>]
    let babelPlugins () : obj[] = failwith "JS"
    
    [<Emit("Object.keys($0)")>]
    let objectKeys (o:obj) : string [] = failwith "JS"

    [<Emit("$1[$0]")>]
    let getProperty (s:string) (o:obj) : obj = failwith "JS"

    /// Eval code and return the result together with exports from the code
    [<Emit("(function() { var exports = {}; var res = eval($0); return [exports, res]; })()")>]
    let eval (s:string) : obj * obj = failwith "JS"

    [<Emit("Babel")>]
    let babel : Babel = Unchecked.defaultof<_> 

// --------------------------------------------------------------------------------------
// Calling Fable service
// --------------------------------------------------------------------------------------

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

module Fable = 
    let mutable root = ""

    let compile text = promise {
        Log.trace("fsi", "Calling %s", root + "/fable")
        if root = "" then failwith "Root URL for services not set!"
        let! res = 
          Globals.axios().post(root + "/fable", text)
          |> Promise.fail (fun e -> 
                Log.exn("fsi", "Calling %s failed: %O", root + "/fable", e)
                failwith "Calling autocomplete service failed" )
        return res.data |> unbox<FableResult> }


// --------------------------------------------------------------------------------------
// F# Interactive session - keeping track of declarations
// --------------------------------------------------------------------------------------

type FsiGlobals = Map<string, (string * obj)>

type Globals() = 

    /// Keeps the state of the FSI session 
    /// (declared variables, their types and values)
    let mutable globals : FsiGlobals = Map.empty

    member x.ReadDeclarations(decls:Declaration[], exports:obj) =
        decls |> Array.map (fun d -> d, JsInterop.getProperty d.name exports)
    
    member x.AddDeclarations(decls:(Declaration * obj)[]) =
        globals <- decls |> Array.fold (fun globals (d, v) ->
            Log.trace("fsi", "Adding declaration %s:%s = %O", d.name, d.``type``, v)
            Map.add d.name (d.``type``, v) globals) globals

    /// Generate F# code that provides globals in scope when running
    /// selection. The code is injected when compiling expression.
    member x.GetBindings varName = 
      [ if not (Map.isEmpty globals) then
          yield "module Globals = "
          for d in globals do 
            yield "  [<Fable.Core.Emit(\"" + varName + "." + d.Key + "\")>]"
            yield "  let " + d.Key + " : " + fst d.Value + " = failwith \"JS\"" 
          yield "open Globals\n" ] |> String.concat "\n"

    /// Returns JavaScript object with globals bindings
    member x.GetObject() = 
        JsInterop.createObj [ for kv in globals -> kv.Key, snd kv.Value ]

// --------------------------------------------------------------------------------------
// Handling of Monaco Zones
// --------------------------------------------------------------------------------------

open System.Collections.Generic

type Zones(ed:editor.ICodeEditor) = 

    /// All the zones we created
    let mutable allZones = []
    /// The top of a zone (so that we can remove all zones below given Top location)
    let zoneTops = Dictionary<float, float>()

    /// Remove all zones from the editor
    member x.RemoveAllZones() =
        ed.changeViewZones(fun accessor ->
            for id, _ in allZones do accessor.removeZone(id)
            zoneTops.Clear()
            allZones <- [] )

    /// Creates a new zone and adds it to the collection
    member x.CreateAndAddZone(startLine, endLine, col) =        
        Log.trace("fsigui", "Creating zone at (%s, %s)", endLine, col)
        let mutable zoneId = -1.
        let zone = JsInterop.createEmpty<editor.IViewZone>
        let top = ed.getTopForPosition(startLine, col)

        let node = Browser.document.createElement_div()
        let wrapper = Browser.document.createElement_div()
        wrapper.className <- "fsi-wrapper"
        node.appendChild(wrapper) |> ignore
        node.style.background <- "#f0f0f0"
        ed.changeViewZones(fun accessor ->  
            // Remove all zones below the current location ("invalidate them")
            let remove, stay = allZones |> List.partition (fun (id, z) -> zoneTops.[id] >= top)
            allZones <- stay
            for id, z in remove do
                Log.trace("fsigui", "Removing zone %s", id)
                accessor.removeZone(id)

            // Create and add the current zone
            zone.afterLineNumber <- endLine
            zone.heightInPx <- Some 1.0
            zone.domNode <- node
            zoneId <- accessor.addZone(zone) 
            allZones <- (zoneId, zone) :: allZones
            zone.onDomNodeTop <- Some(System.Func<_, _>(fun t -> zoneTops.[zoneId] <- t))
            () )
        zoneId, zone, wrapper


/// Captures different things that evaluation can produce
type Output = 
    | Declaration of name:string * typ:string * value:obj
    | PrintedOutput of string
    | ItValue of value : obj


/// Run compiled Fable code and return results & declarations
let private runCompiledCode decls (code:string) (globals:Globals) = 
    let (?) = Fable.Core.JsInterop.(?)
    let results = ResizeArray<_>()
    
    // Careful here - we need to make sure everything that 'eval' might need
    // is in scope here. Including _fableCore (hence ignore below) and values
    // nmed 'consoleLog' (to caputre printf) and 'hiddenIonideGlobals'.
    ignore List.head 
    let consoleLog = System.Func<_, _>(fun s -> results.Add(PrintedOutput s))
    let code = code.Replace("console.log", "consoleLog")         
    let hiddenIonideGlobals = globals.GetObject()
    Log.trace("fsi", "Evaluating compiled code: %s\nGlobals: %O", code, hiddenIonideGlobals)

    // Eval code, store exported declarations & return bindings + it value
    let exports, it = JsInterop.eval(code)
    let bindings = globals.ReadDeclarations(decls, exports)
    globals.AddDeclarations(bindings)
    for d, v in bindings do results.Add(Declaration(d.name, d.``type``, v))
    if it <> null && (box it?__esModule) = null then
        results.Add(ItValue it)
    results |> List.ofSeq

let private formatValue (value:obj) = 
    text (string value)

/// Message formatting is used in 'appendMessages' and 'appendResults'
let private formatMessages (messages:Message[]) = 
    h?ul ["class" => "messages"] [
        for msg in messages -> 
            h?li ["class" => msg.kind] [ 
                h?span ["class" => "kind"] [text msg.kind]
                h?span ["class" => "msg"] [text msg.message] ] ]

/// Print error messages to the output
let private appendMessages (element:Browser.HTMLElement) (messages:Message[]) = 
    h?div ["class" => "fsi-output"] [ 
        h?h3 [] [text "Error messages"]
        formatMessages messages 
      ] |> renderTo element

/// Print the results of evaluation into a given HTML element, using `formatValue` for values
let private appendResults (element:Browser.HTMLElement) (messages:Message[]) results = 
    let prints = results |> List.choose (function PrintedOutput(s) -> Some(s) | _ -> None) 
    let decls = results |> List.choose (function Declaration(n, t, v) -> Some(n, t, v) | _ -> None) 
    let itval = results |> List.tryPick (function ItValue(v) -> Some(v) | _ -> None) 

    // Output declarations in a style-able list
    let declarations() =
        h?ul ["class" => "declarations"] [
            for n, t, v in decls -> 
                h?li [] [ 
                    h?span ["class" => "sep"] [text "val"]
                    h?span ["class" => "name"] [text n]
                    h?span ["class" => "sep"] [text ":"]
                    h?span ["class" => "typ"] [text t]
                    h?span ["class" => "sep"] [text "="]
                    formatValue v ] ]

    // Output printed text, declarations and the IT value              
    h?div ["class" => "fsi-output"] [
        if not (Array.isEmpty messages) then 
            yield h?h3 [] [text "Error messages"]
            yield formatMessages messages
        if not (List.isEmpty decls) then
            yield h?h3 [] [text "Declarations"]
            yield declarations()            
        for p in prints do
            yield h?h3 [] [text "Program output"]
            yield h?p [] [ text p ]
        match itval with 
        | Some v -> 
            yield h?h3 [] [text "Returned value"]
            yield h?p ["class" => "it"] [ formatValue v ]
        | _ -> ()
    ] |> renderTo element

 
/// Specify the Fable service that is called to compile code to JS
let registerService root =
    Fable.root <- root

/// Interactive service exposes function for setting 'test callback'
/// (this is a Try F# speciality and should probably be separated) 
type IInteractiveService =
  abstract SetTestCode : string * (unit -> unit) -> unit

/// Add F# Interactive support to a given editor component.
/// You also need to call 'registerService' to make this work!
let createInteractiveService (ed:editor.ICodeEditor) =
    let zones = Zones(ed)
    let globals = Globals()
    let mutable test = None

    ignore <| ed.onKeyDown(fun e ->   
      if e.keyCode = KeyCode.Escape then
          // This might need better keybinding
          zones.RemoveAllZones()

      elif e.altKey && e.keyCode = KeyCode.Enter then 
        
        // Add zone below the end of selection 
        // (minus one if last line of selection is empty)
        let sel = ed.getSelection()
        let lineStr = ed.getModel().getLineContent(sel.endLineNumber)
        Log.trace("fsigui", "Current line (%d): '%s', end column: %s", sel.endLineNumber, lineStr, sel.endColumn)
        let lineStr = lineStr.Substring(0, int sel.endColumn - 1)
        let line, col = 
            if System.String.IsNullOrWhiteSpace(lineStr) && sel.endLineNumber > sel.startLineNumber 
            then sel.endLineNumber - 1., 0. else sel.endLineNumber, 0.
        let zoneId, zone, wrapper = zones.CreateAndAddZone(sel.startLineNumber, line, col)

        // Get selection, add declared globals and send to Fable
        let text = ed.getModel().getValueInRange(unbox sel)
        let text = globals.GetBindings("hiddenIonideGlobals") + text 
        Log.trace("fsi", "Sending code to Fable: %s", text)
        Fable.compile text 
        |> Promise.success (fun res ->
            Log.trace("fsi", "Fable returned: %O", res)
            
            match test with Some(_, f) -> f() | _ -> ()

            let opts = { BabelOptions.presets = [| "es2015" |]; plugins = JsInterop.babelPlugins() }
            let messages = res.messages |> Array.filter (fun msg -> 
                // This error message is implicitly ignored
                not (msg.message.Contains("The result of this expression is implicitly ignored")))
            if null = box res.compiled then
                appendMessages wrapper messages
            else
                let code = JsInterop.babel.transformFromAst(res.compiled, text, opts)
                Log.trace("fsi", "Babel returned: %O", code)
                let results = runCompiledCode res.declarations code.code globals
                appendResults wrapper messages results

            // Update zone size after rendering everything
            zone.heightInPx <- Some wrapper.clientHeight
            ed.changeViewZones(fun a -> a.layoutZone(zoneId)) ) |> ignore)

    { new IInteractiveService with
        member x.SetTestCode(t, f) = 
          if t = null then test <- None
          else test <- Some(t, f) }