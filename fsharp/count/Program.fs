// least F# way to count
(*let mutable n = 1

while n <= 10 do
    printfn "%d" n
    n <- n + 1
*)

(* The functional way *)
let rec count n limit = 
    if n <= limit then
        printfn "%d" n
        count (n+1) limit
        

count 1 30000