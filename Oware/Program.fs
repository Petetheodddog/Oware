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

let getSeeds n board = failwith "Not implemented"

let useHouse n board = failwith "Not implemented"

let start position = failwith "Not implemented"

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    let startgame = start North

    //printfn "Hello from F#!"
    0 // return an integer exit code
