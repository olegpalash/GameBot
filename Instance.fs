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

type GetRecord = {
    Address: string
    Delay:   int
    SubTask: SubTaskID
}

type PostRecord = {
    Address: string
    Delay:   int
    SubTask: SubTaskID
    Data:    Map<string, string>
}

type Action = 
    | Get        of GetRecord
    | Post       of PostRecord
    | SetSubTask of SubTaskID
    | SetTask    of TaskID
    | PushTask   of TaskID
    | PullTask
    | Error

    override this.ToString () = 
        match this with
        | Get data        -> $"Get Addr='{data.Address}' Delay={data.Delay} SubTask={data.SubTask}"
        | Post data       -> $"Post Addr='{data.Address}' Delay={data.Delay} SubTask={data.SubTask} Data={data.Data}"
        | SetSubTask sub  -> $"SetSubTask {sub}"
        | SetTask taskid  -> $"SetTask {taskid}"
        | PushTask taskid -> $"PushTask {taskid}"
        | PullTask        -> "PullTask"
        | Error           -> "Error"

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
    Data:            'TData
}

type Instance<'TData>(client: Client, logger: Logger, defaultTask: Task<'TData>, defaultAddr: string, initTask: Task<'TData>) =
    let mutable state = {Task = initTask; SubTask = SubTaskID 0; Response = None}
    let mutable saved : State<'TData> list = []
    let mutable tasks : Map<TaskID, Task<'TData>> = Map []

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
        logger $"{now}\t{task}\t{subtask}\t{str}"

    let handleInternalError str = 
        log $"An internal error occurred: {str}"

        let response = client.Get defaultAddr
        state <- {Task = defaultTask; SubTask = SubTaskID 0; Response = conv response}
        saved <- []

    let doGet (data : GetRecord) = 
        Thread.Sleep data.Delay

        let response = client.Get data.Address
        state <- {state with SubTask = data.SubTask; Response = conv response}

    let doPost (data : PostRecord) = 
        Thread.Sleep data.Delay

        let response = client.Post(data.Address, data.Data)
        state <- {state with SubTask = data.SubTask; Response = conv response}

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
        | Get data -> 
            doGet data
        | Post data ->
            doPost data
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

    member this.Run () = 
        let action = state.Task.Fun (state, log)
        log (action.ToString ())
        doAction action

        this.Run ()

    member this.RegisterTask taskid task =
        tasks <- tasks |> Map.add taskid task
