module fsbase64kata

open System
open System.Text

let base64Table = [| 'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/' |]

let encode (s:string) =
    let rec encodeInternal (input : byte[]) (encoded : byte[]) (i : int) (ti : int) : byte[] =
        let length = input.Length
        if i >= length then encoded
        else
            let a = input.[i]
            let b = if i+1 < length then input.[i+1] else 0uy
            let c = if i+2 < length then input.[i+2] else 0uy

            // 000000_00|0000_0000|00_000000
            encoded.[ti] <- a >>> 2
            encoded.[ti+1] <- ((a &&& 3uy) <<< 4) ||| (b >>> 4)
            encoded.[ti+2] <- (b &&& 15uy) <<< 2 ||| (c >>> 6)
            encoded.[ti+3] <- c &&& 63uy
            encodeInternal input encoded (i+3) (ti+4)

    let input = s |> Encoding.UTF8.GetBytes
    let inputLength = input.Length
    let outputLength = 4 * int(Math.Ceiling(float(inputLength) / 3.0))
    let padding = match inputLength % 3 with
                  | 0 -> 0
                  | r -> 3 - r
    let encoded = encodeInternal input (Array.zeroCreate outputLength) 0 0
    let result = Array.create outputLength '='
    for i = 0 to outputLength - padding - 1 do
        let b = encoded.[i]
        result.[i] <- base64Table.[int(b)]

    String(result)

let runTest input expected =
    let actual = encode input
    match actual = expected with
    | true -> printfn "[PASS]: '%s' -> '%s' == '%s'" input actual expected
    | false -> printfn "[FAIL]: '%s' -> '%s' <> '%s'" input actual expected

[<EntryPoint>]
let main _ =
    runTest "Man" "TWFu"
    runTest "M" "TQ=="
    runTest "Ma" "TWE="
    runTest "any carnal pleasure." "YW55IGNhcm5hbCBwbGVhc3VyZS4="
    runTest "any carnal pleasure" "YW55IGNhcm5hbCBwbGVhc3VyZQ=="
    runTest "any carnal pleasur" "YW55IGNhcm5hbCBwbGVhc3Vy"
    runTest "any carnal pleasu" "YW55IGNhcm5hbCBwbGVhc3U="
    runTest "any carnal pleas" "YW55IGNhcm5hbCBwbGVhcw=="
    runTest "pleasure." "cGxlYXN1cmUu"
    runTest "leasure." "bGVhc3VyZS4="
    runTest "easure." "ZWFzdXJlLg=="
    runTest "asure." "YXN1cmUu"
    runTest "sure." "c3VyZS4="

    Console.WriteLine("Hit any key")
    Console.ReadKey (true) |> ignore
    0
