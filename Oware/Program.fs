module Oware
//--------------------------------------Types--------------------------
type StartingPosition =
  | North
  | South

type State =
  | Playing
  | SouthWon
  | NorthWon
  | Draw

type Board = {
  Player:StartingPosition

  Game:(int*int*int*int*int*int*int*int*int*int*int*int)

  Score:(int*int) //(North, South) respectively

  House:int
}
//--------------------------------------End Types--------------------------

(*getSeeds, which accepts a House number and a Board, and returns the number of
seeds in the specified House*)
let getSeeds n board = 
  let (a,b,c,d,e,f,g,h,i,j,k,l) = board
  match n with 
  |1 -> a
  |2 -> b
  |3 -> c
  |4 -> d
  |5 -> e
  |6 -> f
  |7 -> g
  |8 -> h
  |9 -> i
  |10 -> j
  |11 -> k
  |12 -> l
  |_ -> failwith "invalid House"


//sets the chosen house to 0 so its seed(s) can be distributed later
//take in a board and a house number to modify
let setHouseZero n board =
  let (a,b,c,d,e,f,g,h,i,j,k,l) = board
  match n with
  |1 -> (0,b,c,d,e,f,g,h,i,j,k,l)
  |2 -> (a,0,c,d,e,f,g,h,i,j,k,l)
  |3 -> (a,b,0,d,e,f,g,h,i,j,k,l)
  |4 -> (a,b,c,0,e,f,g,h,i,j,k,l)
  |5 -> (a,b,c,d,0,f,g,h,i,j,k,l)
  |6 -> (a,b,c,d,e,0,g,h,i,j,k,l)
  |7 -> (a,b,c,d,e,f,0,h,i,j,k,l)
  |8 -> (a,b,c,d,e,f,g,0,i,j,k,l)
  |9 -> (a,b,c,d,e,f,g,h,0,j,k,l)
  |10 -> (a,b,c,d,e,f,g,h,i,0,k,l)
  |11 -> (a,b,c,d,e,f,g,h,i,j,0,l)
  |12 -> (a,b,c,d,e,f,g,h,i,j,k,0)
  |_ -> failwith "Invalid House Number"

//Adds a seed to a specified house, will be used l8r
//takes in a house number to ++ 
let incrementHouse n board =
  let (a,b,c,d,e,f,g,h,i,j,k,l) = board
  match n with 
    |1 -> (a+1,b,c,d,e,f,g,h,i,j,k,l)
    |2 -> (a,b+1,c,d,e,f,g,h,i,j,k,l)
    |3 -> (a,b,c+1,d,e,f,g,h,i,j,k,l)
    |4 -> (a,b,c,d+1,e,f,g,h,i,j,k,l)
    |5 -> (a,b,c,d,e+1,f,g,h,i,j,k,l)
    |6 -> (a,b,c,d,e,f+1,g,h,i,j,k,l)
    |7 -> (a,b,c,d,e,f,g+1,h,i,j,k,l)
    |8 -> (a,b,c,d,e,f,g,h+1,i,j,k,l)
    |9 -> (a,b,c,d,e,f,g,h,i+1,j,k,l)
    |10 -> (a,b,c,d,e,f,g,h,i,j+1,k,l)
    |11 -> (a,b,c,d,e,f,g,h,i,j,k+1,l)
    |12 -> (a,b,c,d,e,f,g,h,i,j,k,l+1)
    |_ -> failwith "Invalid House Number"


//Adds the seed(s) from a house to the next houses
//takes in a house number to ++ 
let distributeSeeds n board seeds =
  let board = setHouseZero n board
  let rec distribute board n seeds = 
    match seeds > 0 with 
    | false -> board
    | true -> let board = incrementHouse (n+1) board
              let n = n+1
              let seeds = seeds-1
              distribute board n seeds
  distribute board n seeds
//I don't think that distributeSeeds function is functional just yet.  



(*
useHouse: accepts a House number and a Board, and makes a move using
that House.
*)
let useHouse n board = //failwith "Not implemented"
  distributeSeeds n board (getSeeds n board)

//getSeeds, count the seeds and itt. through them to distribute to Houses greater than the orig, can't use foe's House
//
//          Psuedo Code
//
//get the number of seeds from the given house
//know which players turn it is
//set that house to zero seeds
//add 1 seed to each house after it
//check if the total of the last house sums to 2 or 3
//if it is check the one before it, do this recursivly untill != (2 or 3).
//add the points to the player and remove them from the game
//
// N.B. you CAN'T take seeds from your own side
//
//         end psuedo code

(*
start: accepts a StartingPosition and returns an initialized game where the
person in the StartingPosition starts the game
*)
let start position = //failwith "Not implemented"
  let b = {
    Player=position;
    Game=(4,4,4,4,4,4,4,4,4,4,4,4);
    Score=(0,0);
    House=0
  }
  b.Game


(*
Score: accepts a Board and gives back a tuple of (southScore , northScore)
*)
let score board = failwith "Not implemented"


(*
gameState: accepts a Board and gives back a string that tells us about the
state of the game. Valid strings are “South’s turn”, “North’s turn”, “Game ended in a
draw”, “South won”, and “North won”.
*)
let gameState board = failwith "Not implemented"


[<EntryPoint>]
let main _ =
    let startgame = start North

    //printfn "Hello from F#!"
    0 // return an integer exit code
