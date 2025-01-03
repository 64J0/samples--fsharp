let handleErrorByThrowingException () = failwith "Throwing this exception"

let handleErrorByReturningOption (input: string) =
    match System.Int32.TryParse input with
    | true, number -> Some number
    | _ -> None

let handleErrorByReturningResult (input: string) =
    match System.Int32.TryParse input with
    | true, number -> Ok number
    | _ -> Error "Could not convert input to integer"

let bind (switchFunction: int -> Result<int, string>) (twoTrackInput: Result<int, string>) : Result<int, string> =
    match twoTrackInput with
    | Ok x -> switchFunction x
    | Error err -> Error err

let cleanupDisposablesWhenThrowingException resource =
    use _ = resource
    handleErrorByThrowingException ()
