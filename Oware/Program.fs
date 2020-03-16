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

(*
useHouse: accepts a House number and a Board, and makes a move using
that House.
*)
let useHouse n board = failwith "Not implemented"
//getSeeds, count the seeds and itt. through them to distribute to Houses greater than the orig, can't use foe's House
//
//
//Psuedo Code
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
//end psuedo code

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
