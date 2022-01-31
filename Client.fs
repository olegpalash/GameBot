namespace GameBot

open System
open System.Net
open System.Net.Http

open System.Collections.Generic

type Client(address: string) = 
    let cookies = new CookieContainer()
    let handler = new HttpClientHandler()
    do handler.CookieContainer <- cookies

    let client = new HttpClient(handler)
    let baseUri = new Uri(address)
    do client.BaseAddress <- baseUri

    member this.Get(path: string) = 
        client.GetAsync path
        |> Async.AwaitTask
        |> Async.RunSynchronously

    member this.Post(path: string, data) = 
        let formData = new FormUrlEncodedContent(data)

        client.PostAsync(path, formData)
        |> Async.AwaitTask
        |> Async.RunSynchronously
