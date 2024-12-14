open System
open System.Text.RegularExpressions

let readInput (filename: string) : string =
    IO.File.ReadAllText filename 

let matchMul (input: string) (pattern: string) : string list =
    Regex.Matches(input, pattern) 
    |> Seq.map (fun m -> m.Value)
    |> Seq.toList

// Return list of int pairs from all mul operations
let extractInts (str: string list) : list<int*int> =
    str 
    |> List.map (fun mul -> mul.Split "," |> Array.toList |> List.map (fun s -> int (s |> String.filter Char.IsDigit))) 
    |> List.map (fun l -> (l[0], l[1]))

// Return a list of int pairs only from the mul operations that should be enabled
let rec condExtractInts (enable: bool) (input: string list) : list<int*int> =
    match input with
    | [] -> []
    | head :: tail ->
        let newEnable =
            if Regex.IsMatch(head, "don't") then false
            elif Regex.IsMatch(head, "do") then true
            else enable
        if Regex.IsMatch(head, "mul") && newEnable then
            extractInts [head] @ condExtractInts newEnable tail 
        else
            condExtractInts newEnable tail

// Multiply ints inside pairs and then add them together
let sumResults (l: list<int*int>) : int =
    l |> List.fold (fun s (a, b) -> s + a*b) 0

let input = readInput "input.txt"
let pattern1 = "mul\([0-9]+,[0-9]+\)"
let pattern2 = "do\(\)|don't\(\)|mul\([0-9]+,[0-9]+\)"

// matchMul input pattern1 |> extractInts |> sumResults |> printfn "%A" // Part 1
matchMul input pattern2 |> condExtractInts true |> sumResults |> printfn "%A" // Part 2


