(*
Demonstrate lazy evaluation. We only evaluate functions as needed.
*)
let noarg =
    printfn "Noarg is called"
    5
let y = noarg
let f x y z =
    printfn "X: %d" x
    printfn "Y: %d" y
    printfn "Z: %d" z

f noarg noarg noarg

let rec really_run x =
    if x <= 10000 then
        noarg
        really_run (x+1) 
really_run 1 


(* can we make a stateful function *)
let onearg x = 
    printfn "One arg"
    (x+7)

printfn "Result: %d" (onearg 1)
printfn "Result: %d" (onearg 1)
(* The above is not-stateful, it is dependent on its parameter *)


(* We can write functions that create other functions 
     This creates a closure for the inc function:
        - Function
        - Symbols in the function's scope
*)
let make_increment n =
    let inc x =
        x + n
    inc

let count5 = make_increment 5
let count10 = make_increment 10
printfn "%d" (count5 0)
printfn "%d" (count5 5)
printfn "%d" (count5 10)
printfn "%d" (count10 0)
printfn "%d" (count10 10)
printfn "%d" (count10 20)


(* What if we have a local mutable variable? *)
let puzzle x = 
    printfn "PUZZLE"
    let mutable count = 0
    let counter x = 
        count <- count + 1
        printfn "%d" count
    counter

let count1 = puzzle 0
let count2 = puzzle 0
count1 0
count1 0
count1 0

count2 0
count2 0
count2 0


(* Anonymous functions *)
let rec rep_call x f =
    if x > 0 then
        f x
        rep_call (x-1) f

rep_call 10 (fun x -> printfn "I am anonymous.")

(fun x -> printfn "Anonymous print of %d" x) 5


(* play with lists *)
let l = [1..10]
printfn "%A" l

l |> List.map (fun x -> x + 10) |> printf "%A"