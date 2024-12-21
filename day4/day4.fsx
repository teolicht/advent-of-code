open System
open System.Text.RegularExpressions

let readInput (filename: string) : string array =
    IO.File.ReadAllLines filename 

let rec charsToString (lists: char list) : string =
    match lists with
    | [] -> "" 
    | head::tail -> (string head) + (charsToString tail)

// List of rows (as strings)
let rows (input: string array) : string list =
    input |> Array.toList

// List of columns (as strings)
let columns (input: string array) : string list =
    input 
    |> Array.toList
    |> List.map Seq.toList
    |> List.transpose
    |> List.map charsToString

// Returns number of matches for "XMAS" and backwards
let matchXmas (input: string list) : int =
    let xmas (str: string) : int =
        let a = Regex.Count(str, "XMAS")
        let b = Regex.Count(str, "SAMX")
        a + b
        
    input |> List.fold (fun s str -> s + xmas(str)) 0

// Given a specific row t, start finding the diagonal from (t,0), in southeast direction
// (t, 0) -> (t+1, 1) -> (t+2, 2) -> ... -> (139, 139)
let diagonalSE1 (t: int) (n: int) (arr: char array2d) : string =
    [t .. n] |> List.mapi (fun i x -> arr[x,i]) |> charsToString

// Given a specific col t, start finding the diagonal from (0, t), in southeast direction
// (0, t) -> (1, t+1) -> (2, t+2) -> ... -> (139-t, 139)
let diagonalSE2 (t: int) (n: int) (arr: char array2d) : string =
    [t .. n] |> List.mapi (fun i x -> arr[i,x]) |> charsToString

// Given a specific row t, start finding the diagonal from (t, 139) in southwest direction
// (t, 139) -> (t+1, 138) -> (t+2, 137) -> ... -> (139, 139-t)
let diagonalSW1 (t: int) (n: int) (arr: char array2d) : string =
    [t .. n] |> List.mapi (fun i _ -> arr[t+i,n-i]) |> charsToString

// Given a specific col t, start finding the diagonal from (0, t) in southwest direction
// (0, t) -> (1, t-1) -> (2, t-2) -> ... -> (139-t, 0)
let diagonalSW2 (t: int) (arr: char array2d) : string =
    [0 .. t] |> List.mapi (fun i _ -> arr[i,t-i]) |> charsToString

// Gather all diagonals
let diagonals (arr: char array2d) =
    let n = arr[0,*].Length-1
    let se1Diagonals = [0 .. n] |> List.map (fun t -> diagonalSE1 t n arr)
    let se2Diagonals = [1 .. n] |> List.map (fun t -> diagonalSE2 t n arr)
    let sw1Diagonals = [0 .. n] |> List.map (fun t -> diagonalSW1 t n arr)
    let sw2Diagonals = [0 .. n-1] |> List.map (fun t -> diagonalSW2 t arr)
    se1Diagonals @ se2Diagonals @ sw1Diagonals @ sw2Diagonals

// Given a specific position in the matrix, determine if it is the center of two crossed
// 'MAS' sequences (which may be in reversed order)
let isCrossMAS (i: int, j: int) (arr: char array2d) : int =
    let mas (str: string) =
        if Regex.IsMatch(str, "MAS") || Regex.IsMatch(str, "SAM") then true
        else false
    // Southeast diagonal
    let diag1 = [ arr[i-1,j-1]; arr[i,j]; arr[i+1,j+1] ] |> charsToString
    // Southwest diagonal
    let diag2 = [ arr[i-1,j+1]; arr[i,j]; arr[i+1,j-1] ] |> charsToString
    if mas diag1 && mas diag2 then 1
    else 0

// Count number of crossed 'MAS'
let countCrossMAS (arr: char array2d) =
    let mutable amount = 0
    for i in [1 .. arr[*,0].Length-2] do 
        for j in [1 .. arr[i,*].Length-2] do
            if arr[i,j] = 'A' then
                amount <- amount + isCrossMAS (i, j) arr
    amount

let input = readInput "input.txt"
let table = input |> Array.toList |> List.map Seq.toList |> array2D 

let diagonalMatches = diagonals table |> matchXmas 
let horizontalMatches = input |> rows |> matchXmas
let verticalMatches = input |> columns |> matchXmas
let totalMatches = diagonalMatches + horizontalMatches + verticalMatches

printfn "Total 'XMAS' matches: %A" totalMatches
printfn "Total cross-'MAS' matches: %A" (countCrossMAS table)
