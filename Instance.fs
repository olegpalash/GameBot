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
    | SetTime    of DateTime option * TaskID
    | Error
    | Stop

    override this.ToString () = 
        match this with
        | Get(addr, sub, delay)        -> $"Get Addr='{addr}' SubTask={sub} Delay={delay}"
        | Post(addr, sub, delay, data) -> $"Post Addr='{addr}' SubTask={sub} Delay={delay} Data={data}"
        | SetSubTask sub               -> $"SetSubTask {sub}"
        | SetTask taskid               -> $"SetTask {taskid}"
        | SetTime(time, taskid)        -> $"SetTime Time={time} Task={taskid}"
        | Error                        -> "Error"
        | Stop                         -> "Stop"

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
    Id:       TaskID
    Fun:      State<'TData> * Logger -> Action
    Enabled:  bool    
    Time:     DateTime option
    Priority: int
    Data:     'TData
}

type Instance<'TData>(client: Client, logger: Logger, defaultTaskId: TaskID, defaultAddr: string, initTaskId: TaskID, tasksList: Task<'TData> list) =
    let tasks =
        tasksList
        |> List.map (fun t -> (t.Id, ref t))
        |> Map    

    let defaultTaskRef = tasks |> Map.find defaultTaskId

    let mutable currTaskRef = tasks |> Map.find initTaskId

    let mutable state = {
        Task     = !currTaskRef
        SubTask  = SubTaskID 0
        Response = None }

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
        currTaskRef <- defaultTaskRef
        state <- {Task = !defaultTaskRef; SubTask = SubTaskID 0; Response = conv response}

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
            currTaskRef <- task
            state <- {state with Task = !task; SubTask = SubTaskID 0}
        | None ->
            handleInternalError $"Invalid task id: {taskid}"

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
        currTaskRef <- defaultTaskRef
        state <- {Task = !defaultTaskRef; SubTask = SubTaskID 0; Response = conv response}

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
        | SetTime(time, taskid) ->
            currTaskRef := {!currTaskRef with Time = time}
            doSetTask taskid
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

    member this.GetNextTask () = 
        let now = DateTime.Now
        let folder st _ taskRef = 
            let task = !taskRef
            if task.Enabled && task.Priority > state.Task.Priority then
                match st, task.Time with
                | Some(_, stime), Some(time) when stime > now && time < stime ->
                    Some(task, time)
                | Some(stask, _), Some(time) when task.Priority > stask.Priority ->
                    Some(task, time)
                | None, Some(time) ->
                    Some(task, time)
                | _ ->
                    st
            else
                st

        tasks
        |> Map.fold folder None
