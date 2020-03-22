module OldSchool.I18n.Lib.Tests.Playground
//https://fsharp.github.io/FSharp.Compiler.Service/untypedtree.html
(*
HOMEDRIVE=C:
HOMEPATH=\Users\Tomek
*)
open System
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharp.Compiler.Ast
open Xunit
open Xunit.Abstractions

module SynBinding =
    let(|Body|) this = 
        let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = this
        body

type TestFixture(hlp:ITestOutputHelper) =
    
    
    // Create an interactive checker instance 
    let checker = FSharpChecker.Create()

    let getUntypedTree (file, input) = 
        // Get compiler options for the 'project' implied by a single script file
        let projOptions, errors = 
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projOptions)

        // Run the first phase (untyped parsing) of the compiler
        let parseFileResults = 
            checker.ParseFile(file, input, parsingOptions) 
            |> Async.RunSynchronously

        match parseFileResults.ParseTree with
        | Some tree -> tree
        | None -> 
            parseFileResults.Errors
            |> Seq.map(sprintf "%A")
            |> String.concat "\n"
            |> failwith

    /// Walk over a pattern - this is for example used in 
    /// let <pat> = <expr> or in the 'match' expression
    //let rec visitPattern = function
    //  | SynPat.Wild(_) -> 
    //      printfn "  .. underscore pattern"
    //  | SynPat.Named(pat, name, _, _, _) ->
    //      visitPattern pat
    //      printfn "  .. named as '%s'" name.idText
    //  | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
    //      let names = String.concat "." [ for i in ident -> i.idText ]
    //      printfn "  .. identifier: %s" names
    //  | pat -> printfn "  .. other pattern: %A" pat

    /// Walk over an expression - if expression contains two or three 
    /// sub-expressions (two if the 'else' branch is missing), let expression
    /// contains pattern and two sub-expressions
    let rec visitExpression = function
      | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
          // Visit all sub-expressions
          printfn "Conditional:"
          visitExpression cond
          visitExpression trueBranch
          falseBranchOpt |> Option.iter visitExpression 

      | SynExpr.LetOrUse(_, _, bindings, body, _) ->
          // Visit bindings (there may be multiple 
          // for 'let .. = .. and .. = .. in ...'
          printfn "LetOrUse with the following bindings:"
          for binding in bindings do
            let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, 
                         data, pat, retInfo, init, m, sp)) = binding
            //visitPattern pat 
            visitExpression init
          // Visit the body expression
          printfn "And the following body:"
          visitExpression body
      | expr -> printfn " - not supported expression: %A" expr

    let visitMember = function
    | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpression expr
    | SynMemberDefn.ImplicitInherit(_, args, _, _) -> visitExpression args
    | SynMemberDefn.LetBindings(bindings, _, _, _) -> 
        for SynBinding.Body body in bindings do visitExpression body
    | SynMemberDefn.Member(SynBinding.Body body, _) -> visitExpression body
    | _ -> ()

    let visitType (SynTypeDefn.TypeDefn(c, repr, members,_)) =
        //SynTypeDefnRepr.ObjectModel(...) = repr
        for m in members do visitMember m

    /// Walk over a list of declarations in a module. This is anything
    /// that you can write as a top-level inside module (let bindings,
    /// nested modules, type declarations etc.)
    let rec visitDeclarations decls = 
      for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) ->
            // Let binding as a declaration is similar to let binding
            // as an expression (in visitExpression), but has no body
            for SynBinding.Body body in bindings do
              //visitPattern pat 
              visitExpression body  
        
        | SynModuleDecl.NestedModule (_, _, decls, _, _) -> 
            visitDeclarations decls
        | SynModuleDecl.DoExpr(_, expr, _) -> 
            visitExpression expr
        | SynModuleDecl.Types(types, _) -> 
            for t in types do
                visitType t
        | _ -> printfn " - not supported declaration: %A" declaration

    /// Walk over all module or namespace declarations 
    /// (basically 'module Foo =' or 'namespace Foo.Bar')
    /// Note that there is one implicitly, even if the file
    /// does not explicitly define it..
    let visitModulesAndNamespaces modulesOrNss =
      for moduleOrNs in modulesOrNss do
        let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = moduleOrNs
        printfn "Namespace or module: %A" lid
        visitDeclarations decls

    // Sample input for the compiler service
    let input =
      //"""
      //let foo() = 
      //  let msg = "Hello world"
      //  if true then 
      //    printfn "%s" msg
      //"""
        """
        type X() =
            let z = 10
            do printf "ddd"

            member _.Z = 2 + 2
            member _.Q() =
                printf "XXX"
        """

    let extract tree =
        // Extract implementation file details
        match tree with
        | ParsedInput.ImplFile(implFile) ->
            // Extract declarations and walk over them
            let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
            visitModulesAndNamespaces modules
        | _ -> failwith "F# Interface file (*.fsi) not supported."

    // File name in Unix format
    let file = "/home/user/Test.fsx"
    [<Fact>]
    member _.Aqq() = 
        let tree = getUntypedTree(file, SourceText.ofString input)
        hlp.WriteLine(sprintf "%A" tree)

        extract tree
        ()