open System

let input = IO.File.ReadAllLines "input.txt"

let trimmedInput = input |> Array.map (fun entry -> entry.Split (" ", StringSplitOptions.RemoveEmptyEntries))
let intInput = trimmedInput |> Array.map (fun a -> (int a[0], int a[1]))

let firstColumn = intInput |> Array.map (fun p -> fst p) 
let secondColumn = intInput |> Array.map (fun p -> snd p)


// PART 1 
let sortedFirstColumn = firstColumn |> Array.sort
let sortedSecondColumn = secondColumn |> Array.sort

let mutable i = 0
let mutable diff = 0
while i <= firstColumn.Length-1 do
    diff <- diff + abs (sortedFirstColumn[i] - sortedSecondColumn[i])
    i <- i + 1

printfn "Total distanceb between lists: %A" diff


// PART 2

let mutable totalSimilarityScore = 0
for i in firstColumn do
    let mutable iFreq = 0
    for j in secondColumn do
        if i = j then
            iFreq <- iFreq + 1
    totalSimilarityScore <- totalSimilarityScore + (i * iFreq)

printfn "Total similarity score: %A" totalSimilarityScore

// VERY imperative, might as well have written it in Python
// Maybe try rewriting to a functional version some day

