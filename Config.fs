namespace GameBot

open System
open System.IO

open FSharp.Data

type Config(map : Map<string, string>) = 
    let tryGetParsed (parser: string -> 'a) key =
        try
            map
            |> Map.find key
            |> parser
            |> Some
        with
            | _ -> None

    member this.ToMap : Map<string, string> = map

    member this.TryGet(key: string): string option = 
        Map.tryFind key map

    member this.Get(key: string, def: string): string = 
        this.TryGet(key)
        |> Option.defaultValue def

    member this.TryGetBool(key : string): bool option = tryGetParsed Boolean.Parse key

    member this.GetBool(key : string, def : bool): bool = 
        this.TryGetBool(key)
        |> Option.defaultValue def

    member this.TryGetFloat(key : string): float option = tryGetParsed Double.Parse key

    member this.GetFloat(key : string, def : float): float = 
        this.TryGetFloat(key)
        |> Option.defaultValue def

    member this.TryGetInt(key : string): int option = tryGetParsed Int32.Parse key

    member this.GetInt(key : string, def : int): int = 
        this.TryGetInt(key)
        |> Option.defaultValue def

    member this.TryGetTime(key : string): TimeSpan option = tryGetParsed TimeSpan.Parse key

    member this.GetTime(key : string, def : TimeSpan): TimeSpan = 
        this.TryGetTime(key)
        |> Option.defaultValue def

    override this.ToString() = map.ToString()

    static member Empty = Config(Map([]))

module CfgLoader = 
    exception InvalidLineException of string

    let load (conf : Config) (path : string) (includeDir : string) = 
        let rec loadFile (map : Map<string, string>) (path : string) = 
            let folder (map, prefix) (line : string) =
                let line = line.Trim()

                if line = "" || line.StartsWith("#") then
                    map, prefix

                elif line.StartsWith("@include") then
                    loadFile map (includeDir + "/" + line.[8 ..].Trim() + ".cfg"), prefix

                elif line.StartsWith("[") then
                    let e = line.IndexOf(']')
                    if e <> -1 then
                        let pref = line.[1 .. e - 1].Trim()
                        if pref = "" then
                            map, ""
                        else 
                            map, (pref + ".")
                    else
                        raise (InvalidLineException($"Invalid line: '{line}'"))

                else
                    let e = line.IndexOf('=')
                    if e <> -1 then
                        let name  = line.[.. e - 1].Trim()
                        let value = line.[e + 1 ..].Trim()

                        Map.add (prefix + name) value map, prefix
                    else
                        raise (InvalidLineException($"Invalid line: '{line}'"))

            let lines = File.ReadAllLines(path)

            lines
            |> Array.fold folder (map, "")
            |> fst

        let map = loadFile conf.ToMap path
        Config(map)

module JsonLoader = 
    let load (conf : Config) (path : string) = 
        let rec folder prefix state (key, value) = 
            match value with
            | JsonValue.Boolean v ->
                Map.add (prefix + key) (string v) state
            | JsonValue.Number v ->
                Map.add (prefix + key) (string v) state
            | JsonValue.Float v ->
                Map.add (prefix + key) (string v) state
            | JsonValue.String v ->
                Map.add (prefix + key) v state
            | JsonValue.Record properties ->
                Seq.fold (folder (prefix + key + ".")) state properties
            | _ ->
                state

        let text = File.ReadAllText(path)
        let json = JsonValue.Parse(text)

        match json with
        | JsonValue.Record properties ->
            let map = conf.ToMap
            let newmap = Seq.fold (folder "") map properties
            Config(newmap)
        | _ ->
            conf

module ConfigLoader = 
    let load (includeDir : string) (names : string list) = 
        let folder conf name = 
            if File.Exists(name + ".cfg") then
                CfgLoader.load conf (name + ".cfg") includeDir
            elif File.Exists(name + ".json") then
                JsonLoader.load conf (name + ".json")
            else
                conf

        names |> List.fold folder Config.Empty
