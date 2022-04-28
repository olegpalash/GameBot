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

type Task = {
    Id:       TaskID
    Fun:      IState -> Action
    Enabled:  bool    
    Time:     DateTime option
    Priority: int
    Data:     obj
}

and IState = 
    abstract Task     : Task
    abstract SubTask  : SubTaskID
    abstract Response : Response option
    abstract Logger   : Logger

    abstract GetTaskById : TaskID -> Task option
    abstract GetNextTask : unit   -> Task option

type Instance(client: Client, logger: Logger, defaultTaskId: TaskID, defaultAddr: string, initTaskId: TaskID, tasksList: Task list) =
    let tasks =
        tasksList
        |> List.map (fun t -> (t.Id, ref t))
        |> Map    

    let defaultTaskRef = tasks |> Map.find defaultTaskId

    let mutable currTaskRef  = tasks |> Map.find initTaskId
    let mutable currSubTask  = SubTaskID 0
    let mutable currResponse = None
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
        let (TaskID task) = currTaskRef.Value.Id
        let (SubTaskID subtask) = currSubTask
        logger $"{now} {task,8} {subtask,2} {str}"

    let handleInternalError str = 
        log $"An internal error occurred: {str}"

        let response = client.Get defaultAddr
        currTaskRef  <- defaultTaskRef
        currSubTask  <- SubTaskID 0
        currResponse <- conv response

    let doGet addr sub (delay : int)  = 
        Thread.Sleep delay

        let response = client.Get addr
        currSubTask  <- sub
        currResponse <- conv response

    let doPost addr sub (delay : int)  data = 
        Thread.Sleep delay

        let response = client.Post(addr, data)
        currSubTask  <- sub
        currResponse <- conv response

    let doSetTask taskid = 
        match Map.tryFind taskid tasks with
        | Some task ->
            currTaskRef <- task
            currSubTask <- SubTaskID 0
        | None ->
            handleInternalError $"Invalid task id: {taskid}"

    let handleError () = 
        let now = DateTime.Now.ToString("yyyy-MM-dd-HHmmss")
        let path = $"errors/{now}.html"

        match currResponse with
        | Some response ->
            File.WriteAllText (path, response.Content)
            log $"An error occurred, page saved to {path}"
        | None ->
            log $"An error occurred, no response page"

        let response = client.Get defaultAddr
        currTaskRef  <- defaultTaskRef
        currSubTask  <- SubTaskID 0
        currResponse <- conv response

    let doAction action = 
        match action with
        | Get(addr, sub, delay) -> 
            doGet addr sub delay
        | Post(addr, sub, delay, data) ->
            doPost addr sub delay data
        | SetSubTask subtask ->
            currSubTask <- subtask
        | SetTask taskid ->
            doSetTask taskid
        | SetTime(time, taskid) ->
            currTaskRef := {!currTaskRef with Time = time}
            doSetTask taskid
        | Error ->
            handleError ()
        | Stop ->
            isRun <- false

    let getNextTask () = 
        let now = DateTime.Now

        let mapping (time : DateTime) (pri : int) = 
            let ticks = time.Ticks - now.Ticks
            if ticks > 0L then
                ticks
            else
                -(int64 pri)

        let folder st _ taskRef = 
            let task = !taskRef
            if task.Enabled && task.Priority > currTaskRef.Value.Priority then
                match st, task.Time with
                | Some(_, _, sval), Some(time) ->
                    let value = mapping time (task.Priority)
                    if value < sval then
                        Some(task, time, value)
                    else
                        st
                | None, Some(time) ->
                    let value = mapping time (task.Priority)
                    Some(task, time, value)
                | _ ->
                    st
            else
                st

        tasks
        |> Map.fold folder None
        |> Option.map (fun (task, _, _) -> task)


    member this.Run () = 
        let action = currTaskRef.Value.Fun (upcast this)
        log (action.ToString ())
        doAction action

        if isRun then
            this.Run ()
        else
            ()

    interface IState with
        member this.Task     = !currTaskRef
        member this.SubTask  = currSubTask
        member this.Response = currResponse
        member this.Logger   = log

        member this.GetTaskById taskid = 
            tasks
            |> Map.tryFind taskid
            |> Option.map (!)

        member this.GetNextTask () = 
            getNextTask ()
