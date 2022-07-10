module GameBot.Interface

open System
open System.IO

open FSharp.Data

let selectElements (tag : string) (state : IState) = 
    match state.Response with
    | Some response ->
        match response.Document with
        | Some doc -> doc.CssSelect tag
        | None     -> []
    | None -> []

let selectElement (tag : string) (state : IState) = 
    state.Response
    |> Option.bind (fun resp -> resp.Document)
    |> Option.bind (fun doc ->
        match doc.CssSelect tag with
        | head :: _ -> Some head
        | []        -> None)

let findAllLinks state = 
    state
    |> selectElements "a"

let findLinkByText (text : string) (lst : HtmlNode list) =
    lst
    |> List.tryFind (fun x -> x.InnerText().Contains(text))

let selectLinkByText text state = 
    state
    |> selectElements "a"
    |> findLinkByText text

let followLink (state : IState) sub delay (node : HtmlNode) =
    state.Logger $"-> {node.InnerText()}"
    
    let href = node.Attribute("href").Value()
    Get(href, sub, delay)

let followAddress (state : IState) sub delay (address : string) =
    state.Logger $"-> {address}"
    
    Get(address, sub, delay)

let rec matchLinks state mapping = 
    let links = findAllLinks state
    match mapping with
    | (text, sub, delay) :: tail ->
        match (findLinkByText text links) with
        | Some node ->
            followLink state sub delay node
        | None ->
            matchLinks state tail
    | [] ->
        Error

let matchLink state text sub delay = 
    let links = findAllLinks state
    match (findLinkByText text links) with
    | Some node ->
        followLink state sub delay node
    | None ->
        Error

let tryGetBody (state : IState) = 
    state.Response
    |> Option.bind (fun x -> x.Document)
    |> Option.bind HtmlDocument.tryGetBody

let getInnerText (state : IState) = 
    state.Response
    |> Option.bind (fun x -> x.Document)
    |> Option.bind HtmlDocument.tryGetBody
    |> Option.map HtmlNode.innerText
    |> Option.defaultValue ""

let saveReport (dir : string) (state : IState) = 
    Directory.CreateDirectory(dir) |> ignore

    let now = DateTime.Now.ToString("yyyy-MM-dd-HHmmss")
    let path = Path.Combine(dir, $"{now} {state.Name}.txt")

    File.WriteAllText(path, getInnerText state)
