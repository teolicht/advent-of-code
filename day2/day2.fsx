open System

let input = IO.File.ReadAllLines "input.txt" // array of strings, each string is a line

// part one 
let trimmedInput = input |> Array.map (fun entry -> entry.Split (" "))
let reports = trimmedInput |> Array.map (fun a -> a |> Array.map (fun x -> int x))

printfn "%A" reports

// check if an array is in ascending order. if it finds a number which is out of order,
// it first tries to see if removing that number (or the one next to it) results in the array being
// sorted. if this succeeds, checkAscending returns (true, 1) where the 'true' means it got the array
// to be ascending, but it took 1 try. this means that the incrementscheck is NOT allowed to remove 
// any numbers from the array. because it has already removed one 'bad level'.
let rec checkSorted (order: string) (a: array<int>) (tries: int) : bool * int * array<int> =
    let mutable _tries = tries    
    if tries > 1 then (false, tries, a) 
    else
        let mutable i = 0
        let mutable sorted = true
        let mutable _a = a
        while sorted && i < a.Length-1 do
            if order = "asc" then
                if a[i] > a[i+1] then
                    if i+1 = a.Length-1 then
                        _a <- a |> Array.removeAt (i+1)
                        _tries <- _tries + 1
                        sorted <- false
                    else
                        _a <- a |> Array.removeAt i
                        _tries <- _tries + 1
                        sorted <- false
            else
                if a[i] < a[i+1] then
                    if i+1 = a.Length-1 then
                        _a <- a |> Array.removeAt (i+1)
                        _tries <- _tries + 1
                        sorted <- false
                    else
                        _a <- a |> Array.removeAt i
                        _tries <- _tries + 1
                        sorted <- false
            i <- i + 1

        if sorted then (true, _tries, _a)
        else
            checkSorted order _a (_tries)


let rec checkInc (a: array<int>) (tries: int) : bool * int * array<int> =
    let mutable _tries = tries
    if tries > 1 then (false, tries, a)
    else
        let mutable i = 0
        let mutable correctIncs = true
        let mutable _a = a
        while correctIncs && i < a.Length-1 do
            // printfn "i: %d --- looking at a[i]=%d and a[i+1]=%d, diff: %d" i a[i] a[i+1] (abs a[i+1]-a[i])
            if (abs (a[i+1] - a[i]) < 1 || abs (a[i+1] - a[i]) > 3) then
                if i+1 = a.Length-1 then
                    _a <- a |> Array.removeAt (i+1)
                    _tries <- _tries + 1
                    correctIncs <- false
                else
                    if [1..3] |> List.contains (abs (a[i+2] - a[i])) then
                        _a <- a |> Array.removeAt (i+1)
                    else
                        _a <- a |> Array.removeAt i
                    _tries <- _tries + 1
                    correctIncs <- false
            i <- i + 1
        if (correctIncs && _tries <= 1) then (true, _tries, _a)
        else
            checkInc _a (_tries)

// sort and then inc -> if they solve in one try -> SAFE
// OTHERWISE: inc and then sort -> if they solve in one try -> SAFE
// OTHERWISE: UNSAFE.

let mutable safeReports = 0
for rep in reports do
    let mutable tries = 0
    let mutable ordered = false
    let mutable _rep = rep

    let (orderedAsc, triesAsc, newAscRep) = checkSorted "asc" _rep tries
    if orderedAsc then
        ordered <- true 
        tries <- triesAsc
        _rep <- newAscRep
    else
        let (orderedDesc, triesDesc, newDescRep) = checkSorted "desc" _rep tries
        if orderedDesc then
            ordered <- true
            tries <- triesDesc
            _rep <- newDescRep
    let (correctIncs, triesInc, newIncRep) = checkInc _rep tries

    if ordered && correctIncs then
        safeReports <- safeReports + 1
        printfn "%A -> safe " rep 
    else 
        // inc and then sort
        tries <- 0
        ordered <- false 
        _rep <- rep
        let (correctIncs, triesInc, newIncRep) = checkInc _rep tries
        let (orderedAsc, triesAsc, newAscRep) = checkSorted "asc" newIncRep tries
        if orderedAsc then
            ordered <- true 
            tries <- triesAsc
            _rep <- newAscRep
        else
            let (orderedDesc, triesDesc, newDescRep) = checkSorted "desc" newIncRep tries
            if orderedDesc then
                ordered <- true
                tries <- triesDesc
                _rep <- newDescRep
            if ordered && correctIncs then
                safeReports <- safeReports + 1
                printfn "%A -> safe" rep
            else
                printfn "%A -> unsafe" rep

        




printfn "number of reports in total: %d" reports.Length
printfn "total number of safe reports: %d" safeReports


// for report in reports do
// let ascSorted = checkSorted "asc" report 0
// let descSorted = checkSorted "desc" report 0
// if fst ascSorted then
// printfn "%A -> %A (ascending)" report ascSorted
// else if fst descSorted then
// printfn "%A -> %A (descending)" report descSorted
// else
//   printfn "%A -> cannot be sorted" report


// CURRENTLY: in some cases, my code is saying that the report is unsafe, when in reality it should be safe.
// [|79; 76; 73; 72; 70; 69; 65|] -> unsafe
// 12 9 5 6 -> unsafe, because it removes 9, however if it removed 5 instead, then it would safe.

// FUNDAMENTAL PROBLEM: i am running sort before running inc. But what if inc can fix the report in one try?
