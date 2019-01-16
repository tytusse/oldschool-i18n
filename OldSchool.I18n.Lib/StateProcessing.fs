﻿//Copyright © 2019 Dominik Pytlewski. Licensed under Apache License 2.0. See LICENSE file for details
module OldSchool.I18n.StateProcessing

open OldSchool.I18n.Parsing
open OldSchool.I18n.Configuration
open FSharp.Data

type ExtractableCollector(fs:System.IO.Abstractions.IFileSystem,i18Class,i18Method) =
    let fileExtensions = [".cs"; ".fs"]
    let extractOne filePath content = 
        content 
        |> if (filePath:string).ToLower().EndsWith(".fs") 
            then (Extractor (fun _ -> ())).ExtractFs i18Class i18Method filePath
            else (Extractor (fun _ -> ())).ExtractCs i18Class i18Method filePath

    let rec extract dir =
        let subdirs = fs.Directory.GetDirectories(dir) |> Seq.map extract |> Seq.concat

        let result = 
            fs.Directory.GetFiles(dir) 
            |> Seq.filter(fun file -> fileExtensions |> List.exists (fun ext -> file.ToLower().EndsWith(ext)))
            |> Seq.map(fun filePath -> filePath |> fs.File.ReadAllText |> extractOne filePath ) 
            |> Seq.concat

        [result; subdirs] |> Seq.concat

    member __.Extract(searchDirs) =
        searchDirs |> List.map extract |> Seq.concat

type FoldedItem = {
    Message : string
    Translated : string
    FoundAt : string[]
}

type TranslationRecord = JsonProvider<"""{
    "items":[{ 
        "m":"original english message to be translated", 
        "t":"translated message / localized", 
        "at":["filepath:lineNo", "filepath:lineNo"]
    },{ 
        "m":"original english message to be translated", 
        "t":"translated message / localized"
    }]
    } """,RootName="Coll">

let trimBaseDir (cfg:Config) (inp:string) =
    //assumes case sensitive paths
    cfg.SearchDirs 
    |> List.tryPick(fun dir -> if inp.StartsWith dir then inp.Substring(dir.Length) |> Some else None)
    |> function
    |Some x -> x.TrimStart(System.IO.Path.DirectorySeparatorChar)
    |_ -> inp

let collectItems fs (cfg:Config) oldMsgToTransl = 
    cfg.I18nMethodName
    |> Seq.collect(fun i18Method ->
        ExtractableCollector(fs, cfg.I18nClassName,i18Method).Extract cfg.SearchDirs)         
    |> Seq.groupBy (fun x -> x.Msg)
    |> Seq.sortBy (fun (x,_) -> x)
    |> Seq.map (fun (msg,all) -> 
        {
            FoldedItem.Message = msg
            Translated = defaultArg (oldMsgToTransl |> Map.tryFind msg) "" 
            FoundAt = 
                if cfg.IncludeAt 
                then all |> Seq.map (fun x -> sprintf "%s:%i" (trimBaseDir cfg x.File) x.Row) |> Array.ofSeq
                else Array.empty
        } )
                
let serializeItemsIntoTextJson res = 
    let items = 
        res 
        |> Seq.map(fun x -> TranslationRecord.Item(x.Message, x.Translated, x.FoundAt)) 
        |> Array.ofSeq
        |> TranslationRecord.Coll
        
    items.JsonValue.ToString()

let importTransl (fs:System.IO.Abstractions.IFileSystem) (cfg:Config) = 
    let outExists = fs.File.Exists cfg.OutputPath

    let updated =
        (if outExists 
            then
                TranslationRecord.Parse(fs.File.ReadAllText cfg.OutputPath).Items
                |> Seq.ofArray
                |> Seq.map (fun x -> x.M, x.T)            
            else [] |> Seq.ofList)

    let additionals =
        cfg.AdditionalTranslationFiles
        |> Seq.collect(fun x ->             
            printfn "including translation from file: %s" x

            TranslationRecord.Parse(fs.File.ReadAllText x).Items
            |> Seq.ofArray
            |> Seq.map (fun x -> x.M, x.T) )

    additionals 
    |> Seq.append updated
    |> Map.ofSeq