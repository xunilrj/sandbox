let mem f = 
    let mutex = new System.Threading.Mutex()
    let mutable value = Option.None
    fun () -> 
        match value with
        | Some(v) -> v
        | None -> 
                mutex.WaitOne() |> ignore
                try
                    let v = f ()
                    value = Some v |> ignore
                    v
                finally
                    mutex.ReleaseMutex()

let Main =
    let f (x:double) = int x
    let g x = x.ToString()
    let compose f g = f >> g;
    let composed1 = f >> g
    let composed2 = g << f
    let composed3 x = x |> f |> g
    let composed4 x = f <| x |> g
    printfn "%s" <| composed1 11.0
    printfn "%s" <| composed2 22.0
    printfn "%s" <| composed3 33.0
    printfn "%s" <| composed4 44.0
    let id x = x
    printfn "%d" <| id 10
    let f1 = compose f id
    let f2 = compose id f
    printfn "%b" <| (=) (f1 11.0) (f2 11.0)
    let slowf = fun () -> 56
    let memslowf = mem slowf
    printfn "%d" <| memslowf ()