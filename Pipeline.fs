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
