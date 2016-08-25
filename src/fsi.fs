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
    ``mutable`` : bool
    argumentTypes : string[]
    ``type`` : string
}

type Types = {
    names : string[]
    code : string 
}

type FableResult = {
    compiled : obj
    declarations : Declaration[]
    types : Types[]
    resultType : string // or null
    messages : Message[]
}

module Fable = 
    let mutable root = ""

    let compile text = promise {
        Log.trace("fsi", "Sending code to Fable (%s/fable): %O", root, JsInterop.createObj [ "code", box text ])
        if root = "" then failwith "Root URL for services not set!"
        let! res = 
          Globals.axios().post(root + "/fable", text)
          |> Promise.fail (fun e -> 
                Log.exn("fsi", "Calling %s failed: %O", root + "/fable", e)
                failwith "Calling autocomplete service failed" )
        Log.trace("fsi", "Fable returned: %O", res)
        return res.data |> unbox<FableResult> }


// --------------------------------------------------------------------------------------
// F# Interactive session - keeping track of declarations
// --------------------------------------------------------------------------------------

type FsiGlobals = Map<string, (Declaration * obj)>

type Globals() = 

    /// Keeps the state of the FSI session 
    /// (declared variables, their types and values)
    let mutable globals : FsiGlobals = Map.empty
    let mutable types = ResizeArray<string[]>()

    member x.ReadDeclarations(decls:Declaration[], exports:obj) =
        decls |> Array.map (fun d -> d, JsInterop.getProperty d.name exports)
    
    member x.AddTypes(codes:seq<string>) = 
        for c in codes do types.Add(c.Split('\n'))        

    member x.AddDeclarations(decls:(Declaration * obj)[]) =
        globals <- decls |> Array.fold (fun globals (d, v) ->
            Log.trace("fsi", "Adding declaration (%s) %s:%s = %O", d.argumentTypes, d.name, d.``type``, v)
            Map.add d.name (d, v) globals) globals

    /// Generate F# code that provides globals in scope when running
    /// selection. The code is injected when compiling expression.
    member x.GetBindings varName = 
      [ for i, ty in Seq.zip { 1 .. types.Count } types do
          yield sprintf "module Types%d = " i
          for l in ty -> "    " + l  
          yield sprintf "\nopen Types%d\n" i
        if not (Map.isEmpty globals) then
          yield "module Globals = "
          for d in globals do 
            let decl, _ = d.Value
            let modifier = if decl.``mutable`` then "mutable " else ""
            if decl.argumentTypes.Length = 0 then 
              yield "  [<Fable.Core.Emit(\"" + varName + "." + d.Key + "\")>]"
              yield "  let " + modifier + d.Key + " : " + decl.``type`` + " = failwith \"JS\"" 
            else
              let macroArgs = 
                  decl.argumentTypes |> Seq.mapi (fun i _ -> sprintf "$%d" i) |> String.concat ","
              let paramArgs = 
                decl.argumentTypes |> Seq.mapi (sprintf "(x%d:%s)") |> String.concat " "
              yield "  [<Fable.Core.Emit(\"" + varName + "." + d.Key + "(" + macroArgs + ")\")>]"
              yield "  let " + modifier + d.Key + " " + paramArgs + " : " + decl.``type`` + " = failwith \"JS\"" 
          yield "\nopen Globals\n" ] |> String.concat "\n"

    /// Same as GetBindings but returns additional bindings for "it" and "output"
    member x.GetTestBindings(testVar, declVar) = 
      x.GetBindings(declVar) + 
        ( [ "module HiddenTest = "
            "  [<Fable.Core.Emit(\"" + testVar + ".it\")>]"
            "  let it:obj = failwith \"JS\" "
            "  [<Fable.Core.Emit(\"" + testVar + ".output\")>]"
            "  let output:string = failwith \"JS\" "
            ""
            "open HiddenTest"
            "" ] |> String.concat "\n" )

    /// Returns JavaScript object with globals bindings
    member x.GetObject() = 
        JsInterop.createObj [ for kv in globals -> kv.Key, snd kv.Value ]

    /// Copies values back from JS object with global bindings
    member x.CopyValuesBack(obj) = 
        globals <- Map.map (fun k (d, v) -> d, JsInterop.getProperty k obj) globals

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
    | Declaration of Declaration * value:obj
    | PrintedOutput of string
    | ItValue of value : obj


/// Run compiled Fable code and return results & declarations
let private runCompiledCode resType decls (code:string) (globals:Globals) = 
    let (?) = Fable.Core.JsInterop.(?)
    let results = ResizeArray<_>()
    
    // Careful here - we need to make sure everything that 'eval' might need
    // is in scope here. Including _fableCore (hence ignore below) and values
    // nmed 'consoleLog' (to caputre printf) and 'hiddenIonideGlobals'.
    ignore List.head 
    let consoleLog = System.Func<_, _>(fun s -> results.Add(PrintedOutput s))
    let code = code.Replace("console.log", "consoleLog")         
    let hiddenIonideGlobals = globals.GetObject()
    Log.trace("fsi", "Evaluating compiled code: %O\nGlobals: %O", 
      JsInterop.createObj [ "code", box code ], hiddenIonideGlobals)

    // Eval code, store exported declarations & return bindings + it value
    let exports, it = JsInterop.eval(code)
    let bindings = globals.ReadDeclarations(decls, exports)
    globals.CopyValuesBack(hiddenIonideGlobals)
    globals.AddDeclarations(bindings)
    
    for d, v in bindings do results.Add(Declaration(d, v))
    // We get "use strict" for unit-returning values because JavaScript
    if resType <> null && resType <> "unit" && it <> null && 
        (box it?__esModule) = null && it <> box "use strict" then results.Add(ItValue it)
    results |> List.ofSeq


/// Run test code and check whehter the result was true
let private runTestCode (code:string) (lastIt:obj) (lastOutput:string) (globals:Globals) = 
    let (?) = Fable.Core.JsInterop.(?)
    
    // [Insert the same careful warning notice as in 'runCompiledCode']
    ignore List.head 
    let hiddenIonideGlobals = globals.GetObject()
    let hiddenTestValues = JsInterop.createObj [ "output", box lastOutput; "it", lastIt ]
    Log.trace("fsi", "Evaluating test code: %O", 
      JsInterop.createObj [ 
        "code", box code; 
        "hiddenTestValues", box hiddenTestValues
        "hiddenIonideGlobals", box hiddenIonideGlobals
      ])

    // Eval code and ignore declarations & check if it returned true
    try 
        let _, it = JsInterop.eval(code)
        unbox it = true
    with e -> 
        Log.trace("fsi", "Evaluation failed: %O", e)
        false


let private formatValue (value:obj) = 
    let str = string value
    if str.StartsWith("html:") then html (str.Substring(5))
    else text str

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
let private appendResults (element:Browser.HTMLElement) (messages:Message[]) results (types:Types[]) = 
    let prints = results |> List.choose (function PrintedOutput(s) -> Some(s) | _ -> None) 
    let decls = results |> List.choose (function Declaration(d, v) -> Some(d, v) | _ -> None) 
    let itval = results |> List.tryPick (function ItValue(v) -> Some(v) | _ -> None) 

    // Output declarations in a style-able list
    let declarations() =
        h?ul ["class" => "declarations"] [
            for n in types |> Seq.collect (fun t -> t.names) ->
                h?li [] [
                    yield h?span ["class" => "sep"] [text "type"]
                    yield h?span ["class" => "name"] [text n]
                    yield h?span ["class" => "sep"] [text "="]
                    yield h?span ["class" => "dotdotdot"] [text "(...)"] ]                    
            for d, v in decls -> 
                let fullTyp = 
                    Seq.append d.argumentTypes [d.``type``] 
                    |> Seq.reduce (fun a b -> a + " -> " + b)
                h?li [] [ 
                    yield h?span ["class" => "sep"] [text "val"]
                    if d.``mutable`` then yield h?span ["class" => "sep"] [text "mutable"]
                    yield h?span ["class" => "name"] [text d.name]
                    yield h?span ["class" => "sep"] [text ":"]
                    yield h?span ["class" => "typ"] [text fullTyp]
                    yield h?span ["class" => "sep"] [text "="]
                    if d.argumentTypes.Length > 0 then yield text "(function)"
                    else yield formatValue v ] ]

    // Output printed text, declarations and the IT value              
    h?div ["class" => "fsi-output"] [
        if not (Array.isEmpty messages) then 
            yield h?h3 [] [text "Error messages"]
            yield formatMessages messages
        if not (List.isEmpty decls) || types.Length > 0 then
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
        // If we did not produce anything at all, print 'no result'
        if messages.Length = 0 && decls.IsEmpty && types.Length = 0 && prints.IsEmpty && itval.IsNone then 
            yield h?h3 [] [text "Returned value"]
            yield h?p ["class" => "it"] [ text "(no result)" ]
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
      if e.altKey && e.keyCode = KeyCode.Backspace then
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
        let opts = { BabelOptions.presets = [| "es2015" |]; plugins = JsInterop.babelPlugins() }
        let text = ed.getModel().getValueInRange(unbox sel)
        let text = globals.GetBindings("hiddenIonideGlobals") + text 
        Fable.compile text 
        |> Promise.success (fun res ->
            let messages = res.messages |> Array.distinctBy (fun m -> m.message) |> Array.filter (fun msg -> 
                // This error message is implicitly ignored
                not (msg.message.Contains("The result of this expression is implicitly ignored")))
            if null = box res.compiled then
                appendMessages wrapper messages
            else
                let code = JsInterop.babel.transformFromAst(res.compiled, text, opts)
                Log.trace("fsi", "Babel compiled code: %O", code)
                globals.AddTypes([ for t in res.types -> t.code ])
                let results = runCompiledCode res.resultType res.declarations code.code globals
                appendResults wrapper messages results res.types

                // Evaluate test code & trigger callback if we get true
                match test with 
                | None -> ()
                | Some(test, f) -> 
                    let test = globals.GetTestBindings("hiddenTestValues", "hiddenIonideGlobals") + test
                    let output = results |> Seq.choose (function PrintedOutput s -> Some s | _ -> None) |> String.concat "\n"
                    let it = results |> Seq.tryPick (function ItValue v -> Some v | _ -> None)
                    Fable.compile test |> Promise.success (fun testRes ->
                      if testRes.compiled = null then
                        Log.trace("fsi", "Test code did not compile")
                      else
                        let code = JsInterop.babel.transformFromAst(testRes.compiled, test, opts)
                        Log.trace("fsi", "Babel compiled test: %O", code)
                        let success = runTestCode code.code it output globals
                        if success then f() ) |> ignore 

            // Update zone size after rendering everything
            zone.heightInPx <- Some wrapper.clientHeight
            ed.changeViewZones(fun a -> a.layoutZone(zoneId)) ) |> ignore ) |> ignore

    { new IInteractiveService with
        member x.SetTestCode(t, f) = 
          if t = null then test <- None
          else test <- Some(t, f) }