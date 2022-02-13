module GameBot.Interface

open FSharp.Data

let selectElements tag state = 
    match state.Response with
    | Some response ->
        match response.Document with
        | Some doc -> doc.CssSelect tag
        | None     -> []
    | None -> []

let selectElement tag state = 
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

let getInnerText state = 
    state.Response
    |> Option.bind (fun x -> x.Document)
    |> Option.bind HtmlDocument.tryGetBody
    |> Option.map HtmlNode.innerText
    |> Option.defaultValue ""
