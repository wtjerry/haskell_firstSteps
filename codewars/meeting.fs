open System

let toName(s: string) =
  let x = s.Split ':'
  x.[1] + ", " + x.[0]
  
let meeting(s: string): string =
  s.Split ';'
    |> Array.map (toName >> (fun x -> x.ToUpper()))
    |> Array.sort
    |> Array.fold (fun acc item -> acc + "(" + item + ")") ""

