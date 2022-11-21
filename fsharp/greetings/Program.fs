let name = 
   printf "What is your name? "
   System.Console.ReadLine()

let greet name = 
   printfn "Hello, %s, nice to meet you." name

greet name