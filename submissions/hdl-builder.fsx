module HDLNand2Tetris

let not16GateBuilder () =
    [0 .. 15]
    |> List.iter (fun n -> printfn "Not (in=in[%i], out=out[%i]);" n n)

not16GateBuilder()

let and16GateBuilder () =
    [0 .. 15]
    |> List.iter (fun n -> printfn "And (a=a[%i], b=b[%i], out=out[%i]);" n n n)

and16GateBuilder()

let or16GateBuilder () =
    [0 .. 15]
    |> List.iter (fun n -> printfn "Or (a=a[%i], b=b[%i], out=out[%i]);" n n n)

or16GateBuilder()

let mux16GateBuilder () =
    [0 .. 15]
    |> List.iter (fun n ->
                  printfn "Mux (a=a[%i], b=b[%i], sel=sel, out=out[%i]);" n n n)

mux16GateBuilder()
