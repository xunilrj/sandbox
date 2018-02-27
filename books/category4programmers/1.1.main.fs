let Main =
    let f (x:double) = int x
    let g x = x.ToString()
    let composed1 = f >> g
    let composed2 = g << f
    let composed3 x = x |> f |> g
    let composed4 x = f <| x |> g
    printfn "%s" <| composed1 11.0
    printfn "%s" <| composed2 22.0
    printfn "%s" <| composed3 33.0
    printfn "%s" <| composed4 44.0