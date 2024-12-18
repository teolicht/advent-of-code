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
    input |> List.fold (fun s str -> s + Regex.Count(str, "XMAS|SAMX")) 0

// Given a specific row t, start finding the diagonal from (t,0), in southeast direction
// (t, 0) -> (t+1, 1) -> (t+2, 2) -> ... -> (139, 139)
let diagonalSE (t: int) (n: int) (arr: char array2d) : string =
    [t .. n] |> List.mapi (fun i x -> arr[x,i]) |> charsToString

// Given a specific row t, start finding the diagonal from (t,0), in northeast direction
// (t, 0) -> (t-1, 1) -> (t-2, 2) -> ... -> (0, 139)
let diagonalNE (t: int) (n: int) (arr: char array2d) : string =
    [t .. n] |> List.mapi (fun i _ -> arr[n-t-i,i]) |> charsToString

// Given a specific row t, start finding the diagonal from (t,139), in southwest direction
// (t, 139) -> (t+1, 138) -> (t+2, 137) -> ... (139, 0)
let diagonalSW (t: int) (n: int) (arr: char array2d) : string =
    [t .. n] |> List.mapi (fun i _ -> arr[i,n-t-i]) |> charsToString

// Given a specific row t, start finding the diagonal from (t,139), in northwest direction 
// (t, 139) -> (t-1, 138) -> (t-2, 137) -> ... (0, 0)
let diagonalNW (t: int) (n: int) (arr: char array2d) : string =
    [t .. n] |> List.mapi (fun i _ -> arr[n-t-i,n-t-i]) |> charsToString

// Gather all diagonals
let diagonals (arr: char array2d) =
    let n = arr[0,*].Length-1
    let seDiagonals = [0 .. n] |> List.map (fun t -> diagonalSE t n arr);
    let neDiagonals = [0 .. n] |> List.map (fun t -> diagonalNE t n arr);
    let swDiagonals = [0 .. n] |> List.map (fun t -> diagonalSW t n arr);
    let nwDiagonals = [0 .. n] |> List.map (fun t -> diagonalNW t n arr)
    seDiagonals @ neDiagonals @ swDiagonals @ nwDiagonals
    

let input = readInput "input.txt"
let table = input |> Array.toList |> List.map Seq.toList |> array2D 

let diagonalMatches = diagonals table |> matchXmas 
let horizontalMatches = input |> rows |> matchXmas
let verticalMatches = input |> columns |> matchXmas

let totalMatches = diagonalMatches + horizontalMatches + verticalMatches
printfn "%A" totalMatches


// PROBLEM: bounds of the diagonals, and also maybe have to start from column and not row
