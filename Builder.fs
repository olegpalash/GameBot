module GameBot.Builder

open GameBot

type Builder = {
    Client:         Client option
    Logger:         Logger option
    DefaultTask:    TaskID option
    DefaultAddress: string
    InitTask:       TaskID option
    TasksList:      Task list
    Config:         Config
    Middleware:     IMiddleware
    Name:           string option
}

let initBuilder = 
    {
        Client         = None
        Logger         = None
        DefaultTask    = None
        DefaultAddress = "/"
        InitTask       = None
        TasksList      = []
        Config         = Config.Empty
        Middleware     = (fun _ -> None)
        Name           = None
    }

let setClient client (builder : Builder) = 
    { builder with Client = Some client }

let setLogger logger (builder : Builder) = 
    { builder with Logger = Some logger }

let setDefaultTask taskid (builder : Builder) = 
    { builder with DefaultTask = Some taskid }

let setDefaultAddress addr (builder : Builder) = 
    { builder with DefaultAddress = addr }

let setInitTask taskid (builder : Builder) = 
    { builder with InitTask = Some taskid }

let setConfig config (builder : Builder) = 
    { builder with Config = config } 

let setMiddleware middleware (builder : Builder) = 
    { builder with Middleware = middleware } 

let setName name (builder : Builder) = 
    { builder with Name = Some name }

let addTask task (builder : Builder) = 
    { builder with TasksList = task :: builder.TasksList }

let buildInstance b = 
    let settings = {
        Client         = b.Client.Value
        Logger         = b.Logger.Value
        DefaultTaskId  = b.DefaultTask.Value
        DefaultAddress = b.DefaultAddress
        InitTaskId     = b.InitTask.Value
        TasksList      = b.TasksList
        Config         = b.Config
        Middleware     = b.Middleware
        Name           = b.Name.Value }

    Instance(settings)
