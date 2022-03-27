module GameBot.Pipeline

open GameBot.Interface

type PipeState<'TData> = {
    State:  State<'TData>
    Logger: Logger
    Action: Action option
}

let initPipe state logger = 
    {State = state; Logger = logger; Action = None}

let tryFun f pstate = 
    match pstate.Action with
    | Some action ->
        pstate
    | None ->
        match f pstate with
        | Some action ->
            {pstate with Action = action}
        | None ->
            pstate

let tryLink text sub delay pstate = 
    match pstate.Action with
    | Some action ->
        pstate
    | None ->
        let link = selectLinkByText text pstate.State
        match link with
        | Some node ->
            let action = followLink pstate.Logger sub delay node
            {pstate with Action = Some action}
        | None ->
            pstate

let tryLinks sub delay texts pstate = 
    let rec findLink texts links =
        match texts with 
        | head :: tail ->
            match (findLinkByText head links) with
            | Some node ->
                Some node
            | None ->
                findLink tail links
        | [] ->
            None

    match pstate.Action with
    | Some action ->
        pstate
    | None ->
        let link = findAllLinks pstate.State |> findLink texts
        match link with
        | Some node ->
            let action = followLink pstate.Logger sub delay node
            { pstate with Action = Some action }
        | None ->
            pstate

let tryLinkFun text f pstate = 
    match pstate.Action with
    | Some action ->
        pstate
    | None ->
        let link = selectLinkByText text pstate.State
        match link with
        | Some node ->
            let action = f pstate node
            {pstate with Action = Some action}
        | None ->
            pstate

let tryCondFun cond f pstate =  
    match pstate.Action with
    | Some action ->
        pstate
    | None ->
        if cond then
            let action = f pstate
            {pstate with Action = Some action}
        else
            pstate

let tryTextFun text f pstate = 
    match pstate.Action with
    | Some action ->
        pstate
    | None ->
        let content = getInnerText pstate.State
        if content.Contains(text) then
            let action = f pstate
            {pstate with Action = Some action}
        else
            pstate

let elseRun f pstate = 
    match pstate.Action with
    | Some action -> action
    | None -> f pstate

let elseReturn value pstate = 
    match pstate.Action with
    | Some action -> action
    | None -> value

let elseError pstate = 
    match pstate.Action with
    | Some action -> action
    | None -> Error
