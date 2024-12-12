open System.IO

let readInput (filename: string) =
    File.ReadAllLines filename 
    |> Array.map (fun l -> l.Split(" ") |> Array.map int |> Array.toList)
    |> Array.toList // list of list of ints (list of reports)

// Checks if two numbers a and b have the corrent increment
let checkIncs (rep: int list) =
    let compare a b : bool =
        let diff = abs (a-b)
        diff >= 1 && diff <= 3
    let pairs = 
        rep
        |> List.pairwise 
        |> List.map (fun (a, b) -> compare a b)
        |> List.filter id 
    pairs.Length+1 = rep.Length

// Checks if all elements are in the same order (ascending or descending)
let checkOrder (rep: int list) (comparator: int -> int -> bool) =
    let pairs = rep 
                |> List.pairwise 
                |> List.map (fun (a, b) -> comparator a b) 
                |> List.filter id 
    match pairs.Length with
    | 0 -> true
    | l when l=rep.Length-1 -> true
    | _ -> false

// Check order and increments for a certain report
let isSafe (rep: int list) : bool =
    checkIncs rep && (checkOrder rep (fun a b -> a > b) || checkOrder rep (fun a b -> a < b))

let isSafeWithDampener (rep: int list) : bool =
    let reportWithoutLevel (targetIndex: int) (currentRep: int list) =
        currentRep
        |> List.indexed 
        |> List.filter (fun (i, _) -> i <> targetIndex)
        |> List.map (fun (_, l) -> l)
    // * To make it faster: could do a while loop that only runs until a safe variation is found
    let foundSafeVariation = 
        rep 
        |> List.mapi (fun i _ -> reportWithoutLevel i rep) // Builds a list containing all the variations of the current rep 
        |> List.map (fun repVar -> isSafe repVar) // Test each variation using isSafe
        |> List.exists id // True if any of the variations is safe
    (isSafe rep) || foundSafeVariation // Also test if original rep is safe

let reports = readInput "input.txt"
reports |> List.map (fun rep -> isSafe rep) |> List.filter id |> List.length |> printfn "# of safe reports without dampener: %A" // Part 1
reports |> List.map (fun rep -> isSafeWithDampener rep) |> List.filter id |> List.length |> printfn "# of safe reports with dampener: %A" // Part 2

