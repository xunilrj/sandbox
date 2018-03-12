let Main =
  let factorial n =
    let rec loop i acc =
        match i with
        | 0 | 1 -> acc
        | _ -> loop (i-1) (acc * i)
    loop n 1
  let erlangB lambda mu C = 
    let ro = lambda / mu
    let factor i = 
      let roi = System.Math.Pow(ro, float(i))
      let ifat = factorial i
      roi/(float ifat)
    let denominator = List.fold (+) 0.0 <| [for x in 0 .. C-1 do yield (factor x)]
    (factor C) / (float denominator)
  let erlangC lambda mu C =
    let fC = float C
    let ro = lambda / mu
    let factor i = 
      let roi = System.Math.Pow(ro, float(i))
      let ifac = factorial i
      roi/(float ifac)
    let numerator = (factor C)*(fC/(fC-ro))
    let denominator1 = List.fold (+) 0.0 <| [for x in 0 .. C-1 do yield (factor x)]
    let denominator2 = (factor C)*(ro/(fC-ro))
    numerator/(denominator1+denominator2)
  let peB l m C =
    printfn "%f %f %d %f" (float l) (float m) C <| erlangB (float l) (float m) C
  let peC l m C = 
    printfn "%f %f %d %f" (float l) (float m) C <| erlangC (float l) (float m) C
  printfn "Increasing C"
  printfn "------------"
  peB 1 1 1
  peB 1 1 2
  peB 1 1 3
  printfn "Increasing Lambda"
  printfn "------------"
  peB 1 5 3
  peB 3 5 3
  peB 5 5 3
  peB 10 5 3
  printfn "Increasing mu"
  printfn "------------"
  peB 5 1 3
  peB 5 3 3
  peB 5 5 3
  peB 5 10 3
  printfn "ErlangC"
  peC 8 12 1
  let N l m C = 
    let fC = float C
    let ro = l / m
    let A = ro/(fC-ro)
    let B = erlangC l m C
    let C = ro
    A*B+C
  let R l m C =
    let fC = float C
    let ro = l / m
    let A = (erlangC l m C) / (m*(fC - ro))
    let B = 1.0/m
    A+B
  printfn "%f" <|  N 8. 12. 1