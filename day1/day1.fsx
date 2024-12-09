open System

let input = IO.File.ReadAllLines "input.txt"

// part one 
let trimmedInput = input |> Array.map (fun entry -> entry.Split (" ", StringSplitOptions.RemoveEmptyEntries))
let intInput = trimmedInput |> Array.map (fun a -> (int a[0], int a[1]))

let firstColumn = intInput |> Array.map (fun p -> fst p) 
let secondColumn = intInput |> Array.map (fun p -> snd p)


// let sortedFirstColumn = firstColumn |> Array.sort
// let sortedSecondColumn = secondColumn |> Array.sort

// let mutable i = 0
// let mutable diff = 0
// while i <= firstColumn.Length-1 do
   //  diff <- diff + abs (sortedFirstColumn[i] - sortedSecondColumn[i])
//     i <- i + 1

// printfn "sorted first column: %A" sortedFirstColumn
// printfn "sorted second column: %A" sortedSecondColumn

// printfn "total diff: %A" diff


// part two

let mutable totalSimilarityScore = 0
for i in firstColumn do
    let mutable iFreq = 0
    for j in secondColumn do
        if i = j then
            iFreq <- iFreq + 1
    totalSimilarityScore <- totalSimilarityScore + (i * iFreq)

printfn "total similarity score: %A" totalSimilarityScore



