module Oware
//--------------------------------------Types--------------------------
type StartingPosition =
  | North
  | South

type Player = {
  score: int
  side: (int*int*int*int*int*int)
}

type Turn = 
  | North 
  | South

type Board = {
  playerNorth: Player
  playerSouth: Player
  PlayerTurn: Turn
}
//--------------------------------------End Types--------------------------

(*getSeeds, which accepts a House number and a Board, and returns the number of
seeds in the specified House*)
let getSeeds n board = 
  let (a,b,c,d,e,f),(a',b',c',d',e',f') = board.playerNorth.side, board.playerSouth.side
  match n with 
  |1 -> a
  |2 -> b
  |3 -> c
  |4 -> d
  |5 -> e
  |6 -> f
  |7 -> a'
  |8 -> b'
  |9 -> c'
  |10 -> d'
  |11 -> e'
  |12 -> f'
  |_ -> failwith "invalid House"


//sets the chosen house to 0 so its seed(s) can be distributed later
//take in a board and a house number to modify
let setHouseZero board n =
  let (a,b,c,d,e,f),(a',b',c',d',e',f') = board.playerNorth.side, board.playerSouth.side
  match n with
  |1 -> {board with playerNorth = {board.playerNorth with side = (0,b,c,d,e,f)}}
  |2 -> {board with playerNorth = {board.playerNorth with side = (a,0,c,d,e,f)}}
  |3 -> {board with playerNorth = {board.playerNorth with side = (a,b,0,d,e,f)}}
  |4 -> {board with playerNorth = {board.playerNorth with side = (a,b,c,0,e,f)}}
  |5 -> {board with playerNorth = {board.playerNorth with side = (a,b,c,d,0,f)}}
  |6 -> {board with playerNorth = {board.playerNorth with side = (a,b,c,d,e,0)}}
  //----------------------------Middle of board----------------------------------------
  |7 -> {board with playerNorth = {board.playerNorth with side = (0,b',c',d',e',f')}}
  |8 -> {board with playerNorth = {board.playerNorth with side = (a',0,c',d',e',f')}}
  |9 -> {board with playerNorth = {board.playerNorth with side = (a',b',0,d',e',f')}}
  |10 -> {board with playerNorth = {board.playerNorth with side = (a',b',c',0,e',f')}}
  |11 -> {board with playerNorth = {board.playerNorth with side = (a',b',c',d',0,f')}}
  |12 -> {board with playerNorth = {board.playerNorth with side = (a',b',c',d',e',0)}}
  |_ -> failwith "Invalid House Number"


//Adds a seed to a specified house, will be used l8r
//takes in a house number to ++ 
let incrementHouse n (a,b,c,d,e,f,a',b',c',d',e',f') =
  match n with 
    |1 -> (a+1,b,c,d,e,f,a',b',c',d',e',f')
    |2 -> (a,b+1,c,d,e,f,a',b',c',d',e',f')
    |3 -> (a,b,c+1,d,e,f,a',b',c',d',e',f')
    |4 -> (a,b,c,d+1,e,f,a',b',c',d',e',f')
    |5 -> (a,b,c,d,e+1,f,a',b',c',d',e',f')
    |6 -> (a,b,c,d,e,f+1,a',b',c',d',e',f')
    //-----------Middle of board-----------
    |7 -> (a,b,c,d,e,f,a'+1,b',c',d',e',f')
    |8 -> (a,b,c,d,e,f,a',b'+1,c',d',e',f')
    |9 -> (a,b,c,d,e,f,a',b',c'+1,d',e',f')
    |10 -> (a,b,c,d,e,f,a',b',c',d'+1,e',f')
    |11 -> (a,b,c,d,e,f,a',b',c',d',e'+1,f')
    |12 -> (a,b,c,d,e,f,a',b',c',d',e',f'+1)
    |_ -> failwith "Invalid House Number"


//Adds the seed(s) from a house to the next houses
//takes in a house number to ++ 
let distributeSeeds n (a,b,c,d,e,f,a',b',c',d',e',f') =
  let rec innerF xs out =
      match xs with
      | [] -> out
      | _::rest -> innerF rest (1::out)
  innerF xs []



(*
useHouse: accepts a House number and a Board, and makes a move using
that House.
*)
let useHouse n board = //failwith "Not implemented"
  let (a,b,c,d,e,f,a',b',c',d',e',f') = board
  match n with
  |1 -> 
  |2 -> 
  |3 -> 
  |4 -> 
  |5 -> 
  |6 -> 
  //----------------------------Middle of board----------------------------------------
  |7 -> 
  |8 -> 
  |9 -> 
  |10 ->
  |11 ->
  |12 ->
  |_ -> failwith "Invalid House Number"

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
let start position = failwith "Not implemented"


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
