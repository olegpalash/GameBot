module GameBot.Builder

open GameBot

type Builder<'TData> = {
    Client:         Client option
    Logger:         Logger option
    DefaultTask:    TaskID option
    DefaultAddress: string
    InitTask:       TaskID option
    TasksList:      Task<'TData> list
}

let initBuilder = 
    {
        Client = None
        Logger = None
        DefaultTask = None
        DefaultAddress = "/"
        InitTask = None
        TasksList = []
    }

let setClient client builder = 
    { builder with Client = Some client }

let setLogger logger builder = 
    { builder with Logger = Some logger }

let setDefaultTask taskid builder = 
    { builder with DefaultTask = Some taskid }

let setDefaultAddress addr builder = 
    { builder with DefaultAddress = addr }

let setInitTask taskid builder = 
    { builder with InitTask = Some taskid }

let addTask task builder = 
    { builder with TasksList = task :: builder.TasksList }

let buildInstance b = 
    let client  = Option.get b.Client
    let logger  = Option.get b.Logger
    let dTaskId = Option.get b.DefaultTask
    let iTaskId = Option.get b.InitTask

    Instance(client, logger, dTaskId, b.DefaultAddress, iTaskId, b.TasksList)
