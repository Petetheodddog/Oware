module Oware
//--------------------------------------Types--------------------------
type StartingPosition =
  | North
  | South

type State =
  | NorthTurn
  | SouthTurn
  | SouthWon
  | NorthWon
  | Draw

type Game = {
  Player:StartingPosition // felt cute, might delete later
  Board:(int*int*int*int*int*int*int*int*int*int*int*int)
  Score:(int*int) //(North, South) respectively
  House:int
  State:State
}
//--------------------------------------End Types--------------------------

(*getSeeds, which accepts a House number and a Board, and returns the number of
seeds in the specified House*)
let  getSeeds n game = 
  let board = game.Board
  let (a,b,c,d,e,f,g,h,i,j,k,l : int) = board
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
let setHouseZero n game =
  let board = game.Board
  let (a,b,c,d,e,f,g,h,i,j,k,l) = board
  let game = 
    {game with Board =
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
                |_ -> failwith "Invalid House Number"}
  game

//Adds a seed to a specified house, will be used l8r
//takes in a house number to ++ 
let incrementHouse n game =
  let board = game.Board
  let (a,b,c,d,e,f,g,h,i,j,k,l) = board
  let game = 
    {game with Board =
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
                |_ -> failwith "Invalid House Number"}
  game

let checkValid hNum game =
  match game.State with 
  | NorthTurn -> match hNum < 7 with
                 | true -> failwith "invalid house, choose one from your own side"
                 | false -> ()
  | SouthTurn -> match hNum >=7 with
                 | true -> ()
                 | false -> failwith "invalid house, choose one from your own side"
  | _ -> ()

let wrapAround hNum = // wrap around when gets to house 12
    match (hNum % 13) with
    | 0 -> 1
    | _ -> hNum
      

// Swaps the turn to next side
let swapTurn game = 
  let game =
    match (game.State) with
    | NorthTurn -> {game with State = SouthTurn}
    | SouthTurn -> {game with State = NorthTurn}
    | _ -> game
  game

(*
useHouse: accepts a House number and a Board, and makes a move using
that House.
*)
let useHouse hNum game = //failwith "Not implemented"
  let numSeeds = (getSeeds hNum game)
  let game = setHouseZero hNum game

  let rec distribute game hNum seeds = 
    let hNum = wrapAround hNum
    match seeds > 0 with 
    | false ->  game
    | true -> let game = incrementHouse (wrapAround (hNum+1)) game
              let seeds = seeds-1
              let hNum = hNum+1
              distribute game hNum seeds
  distribute game hNum numSeeds
 

(*
start: accepts a StartingPosition and returns an initialized game where the
person in the StartingPosition starts the game
*)
let start (position:StartingPosition) = //failwith "Not implemented"
  //let g = {
  {
    Player = position;
    Board = (4,4,4,4,4,4,4,4,4,4,4,4);
    Score = (0,0);
    House = 0; 
    State = match position with 
            | North -> NorthTurn
            | South -> SouthTurn
  }
  //g.Board


(*
Score: accepts a Board and gives back a tuple of (southScore , northScore)
*)
let score game = //failwith "Not implemented"
  game.Score


(*
gameState: accepts a Board and gives back a string that tells us about the
state of the game. Valid strings are “South’s turn”, “North’s turn”, “Game ended in a
draw”, “South won”, and “North won”.
*)
let gameState game = //failwith "Not implemented"
  match game.State with 
  | Draw -> "Game ended in a draw"
  | NorthWon -> "North Won"
  | SouthWon -> "South Won"
  | NorthTurn -> "North's turn"
  | SouthTurn -> "South's turn"
   

(*let gameState game = //failwith "Not implemented"
  match game.State with 
  | NorthTurn -> "North’s turn"
  | SouthTurn -> "South’s turn"
  | Draw -> "Game ended in a draw"
  | NorthWon -> "North Won"
  | SouthWon -> "South Won"*)

(*let playGame numbers =
    let rec play xs game =
        match xs with
        | [] -> game
        | x::xs -> play xs (useHouse x game)
    play numbers (start South)*)

[<EntryPoint>]
let main _ =

    let startgame = (start North)

    //printfn "Hello from F#!"
    0 // return an integer exit code
