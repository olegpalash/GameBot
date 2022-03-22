namespace GameBot

open System
open System.IO
open System.Threading
open System.Net
open System.Net.Http

open FSharp.Data

type TaskID =
    | TaskID of string

    override this.ToString () = 
        match this with
        | TaskID str -> str

type SubTaskID =
    | SubTaskID of int

    override this.ToString () =
        match this with
        | SubTaskID value -> value.ToString ()

type Action = 
    | Get        of address : string * sub :  SubTaskID * delay : int
    | Post       of address : string * sub :  SubTaskID * delay : int * data : Map<string, string>
    | SetSubTask of SubTaskID
    | SetTask    of TaskID
    | PushTask   of TaskID
    | PullTask
    | Error
    | Stop

    override this.ToString () = 
        match this with
        | Get(addr, sub, delay)        -> $"Get Addr='{addr}' SubTask={sub} Delay={delay}"
        | Post(addr, sub, delay, data) -> $"Post Addr='{addr}' SubTask={sub} Delay={delay} Data={data}"
        | SetSubTask sub  -> $"SetSubTask {sub}"
        | SetTask taskid  -> $"SetTask {taskid}"
        | PushTask taskid -> $"PushTask {taskid}"
        | PullTask        -> "PullTask"
        | Error           -> "Error"
        | Stop            -> "Stop"

type Response = {
    Status:   HttpStatusCode
    Content:  string
    Document: HtmlDocument option
}

type Logger = string -> unit

type State<'TData> = {
    Task:     Task<'TData>
    SubTask:  SubTaskID
    Response: Response option
}

and Task<'TData> = {
    Id:              TaskID
    Fun:             State<'TData> * Logger -> Action
    mutable Enabled: bool    
    mutable Time:    DateTime option
    Priority:        int
    Data:            'TData
}

type Instance<'TData>(client: Client, logger: Logger, defaultTask: Task<'TData>, defaultAddr: string, initTask: Task<'TData>) =
    let mutable state = {Task = initTask; SubTask = SubTaskID 0; Response = None}
    let mutable saved : State<'TData> list = []
    let mutable tasks : Map<TaskID, Task<'TData>> = Map []
    let mutable isRun = true

    let conv (response : HttpResponseMessage) = 
        let status = response.StatusCode
        let content = (response.Content.ReadAsStringAsync ()
            |> Async.AwaitTask
            |> Async.RunSynchronously)
        let document = Some (HtmlDocument.Parse content)

        Some {Status = status; Content = content; Document = document}

    let log str = 
        let now = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
        let (TaskID task) = state.Task.Id
        let (SubTaskID subtask) = state.SubTask
        logger $"{now} {task,8} {subtask,2} {str}"

    let handleInternalError str = 
        log $"An internal error occurred: {str}"

        let response = client.Get defaultAddr
        state <- {Task = defaultTask; SubTask = SubTaskID 0; Response = conv response}
        saved <- []

    let doGet addr sub (delay : int)  = 
        Thread.Sleep delay

        let response = client.Get addr
        state <- {state with SubTask = sub; Response = conv response}

    let doPost addr sub (delay : int)  data = 
        Thread.Sleep delay

        let response = client.Post(addr, data)
        state <- {state with SubTask = sub; Response = conv response}

    let doSetTask taskid = 
        match Map.tryFind taskid tasks with
        | Some task ->
            state <- {state with Task = task; SubTask = SubTaskID 0}
        | None ->
            handleInternalError $"Invalid task id: {taskid}"

    let doRestoreState () = 
        match saved with
        | head :: tail ->
            state <- head
            saved <- tail
        | [] -> 
            handleInternalError "No saved state"

    let handleError () = 
        let now = DateTime.Now.ToString("yyyy-MM-dd-HHmmss")
        let path = $"errors/{now}.html"

        match state.Response with
        | Some response ->
            File.WriteAllText (path, response.Content)
            log $"An error occurred, page saved to {path}"
        | None ->
            log $"An error occurred, no response page"

        let response = client.Get defaultAddr
        state <- {Task = defaultTask; SubTask = SubTaskID 0; Response = conv response}
        saved <- []

    let doAction action = 
        match action with
        | Get(addr, sub, delay) -> 
            doGet addr sub delay
        | Post(addr, sub, delay, data) ->
            doPost addr sub delay data
        | SetSubTask subtask ->
            state <- {state with SubTask = subtask}
        | SetTask taskid ->
            doSetTask taskid
        | PushTask taskid ->
            saved <- state :: saved
            doSetTask taskid
        | PullTask ->
            doRestoreState ()
        | Error ->
            handleError ()
        | Stop ->
            isRun <- false

    member this.Run () = 
        let action = state.Task.Fun (state, log)
        log (action.ToString ())
        doAction action

        if isRun then
            this.Run ()
        else
            ()

    member this.RegisterTask(task : Task<'TData>) =
        tasks <- tasks |> Map.add task.Id task

    member this.Tasks = tasks
