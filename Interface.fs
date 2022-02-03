module GameBot.Interface

open FSharp.Data

let selectElements state tag = 
    match state.Response with
    | Some response ->
        match response.Document with
        | Some doc -> doc.CssSelect tag
        | None     -> []
    | None -> []

let findAllLinks state = 
    selectElements state "a"

let findLinkByText (text : string) (lst : HtmlNode list) =
    lst
    |> List.tryFind (fun x -> x.InnerText().Contains(text))

let followLink logger sub delay (node : HtmlNode) =
    logger $"-> {node.InnerText()}"
    
    let href = node.Attribute("href").Value()
    Get(href, sub, delay)

let rec matchLinks state logger mapping = 
    let links = findAllLinks state
    match mapping with
    | (text, sub, delay) :: tail ->
        match (findLinkByText text links) with
        | Some node ->
            followLink logger sub delay node
        | None ->
            matchLinks state logger tail
    | [] ->
        Error

let matchLink state logger text sub delay = 
    let links = findAllLinks state
    match (findLinkByText text links) with
    | Some node ->
        followLink logger sub delay node
    | None ->
        Error
