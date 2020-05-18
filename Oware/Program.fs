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
  Player:StartingPosition
  Board:(int*int*int*int*int*int*int*int*int*int*int*int)
  Score:(int*int) //(South, North) respectively
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

//wrap around when gets to house 12
let wrapAround hNum = 
    match (hNum % 13) with
    | 0 -> 1
    | _ -> hNum 

//swaps the turn to next side
let swapTurn game = 
  let game =
    match (game.State) with
    | NorthTurn -> {game with State = SouthTurn; Player = South}
    | SouthTurn -> {game with State = NorthTurn; Player = North}
    | _ -> game
  game

//used to check if player is selecting house from oppponents side
let oppSide hNum game = 
  match game.Player with 
  | North -> match (hNum < 7) with 
             | true -> true
             | false -> false
  | South -> match (hNum >= 7) with 
             | true -> true
             | false -> false

//need to check if either side has 0 seeds.
let sumSide game = 
  let (a,b,c,d,e,f,a',b',c',d',e',f') = game.Board
  let southSeeds = a+b+c+d+e+f
  let northSeeds = a'+b'+c'+d'+e'+f'
  (southSeeds, northSeeds)

//captures the seeds from a house, uses game and saves a spare copy in spareGame, incase starvation occurs
let rec captureSeeds hNum game spareGame=
  let seeds = getSeeds hNum game
  let (s,n) = game.Score
  let game = (setHouseZero hNum game)
  let game = 
    {game with 
      Score = 
        match game.Player with
        | North -> (s, n+seeds)
        | South -> (s+seeds, n)}

  //change value of hNum to check for more captureable seeds
  let hNum =               
    match hNum-1 with 
    | 0 -> 12
    | _ -> hNum-1
    
  match (getSeeds hNum game), (oppSide hNum game) with
  | (2|3), true -> captureSeeds hNum game spareGame
  | _, _ -> match game.Player, (sumSide game) with // this match checks that the move made does not remove all the seeds from opponents side
            | _, (0, 0) -> game                    // if it does, then revert to not capturing the seeds and leave the game to continue
            | North, (0, _) -> spareGame           
            | South, (_, 0) -> spareGame
            | _, _ -> game

//gets the current state of the game, in particular the end states: SouthWon, NorthWon & Draw
let getState game = 
   let (s,n) = game.Score
   match s > 24 with // this match checks if south has captured more than 24 seeds and won
   |true -> let game = {game with State = SouthWon}
            game
   |false -> match (n > 24)  with  // match checks if north has captured more than 24 seeds and won
             | true -> let game = {game with State = NorthWon}
                       game
             | false -> match s = 24 && n = 24 with // this match checks if the game has ended in a draw
                        |true ->  let game = {game with State = Draw}
                                  game
                        |false -> game

// distributes all the seeeds from a house to the rest of the houses
let rec distribute game hNum seeds oHouse spareGame = 
                        let hNum = wrapAround hNum
                        match seeds > 0 with //till no more seeds are left to distribute 
                        | false ->  match game.Player, (sumSide game) with
                                    | North, (0, _) -> swapTurn spareGame           
                                    | South, (_, 0) -> swapTurn spareGame
                                    | _, _ -> match (getSeeds hNum game), (oppSide hNum game) with //this match checks if seeds can be captured
                                              | (2|3), true -> captureSeeds hNum game game
                                              | _, _ -> game
                        | true -> match (oHouse = hNum+1) with //this match skips original house when sowing seeds
                                  | true -> let game = incrementHouse (wrapAround (hNum+2)) game
                                            let seeds = seeds-1
                                            let hNum = hNum+2
                                            distribute game hNum seeds oHouse spareGame
                                  | false -> let game = incrementHouse (wrapAround (hNum+1)) game
                                             let seeds = seeds-1
                                             let hNum = hNum+1
                                             distribute game hNum seeds oHouse spareGame

(*
useHouse: accepts a House number and a Board, and makes a move using
that House.
*)
let useHouse hNum game = 
  let oHouse = hNum  //tracks the original house number in case seeds make it all around the board
  match (oppSide hNum game) with //check that player is not manipulating opponents house
  | true -> game
  | false ->  match (getSeeds hNum game) with //check that a house has seeds to sow
              | 0 ->  game
              | _ ->  let spare = game  //spare copy of game in case a rollback on the move is needed
                      let numSeeds = (getSeeds hNum game)
                      let game = setHouseZero hNum game
                      
                      getState (swapTurn (distribute game hNum numSeeds oHouse spare))


(*
start: accepts a StartingPosition and returns an initialized game where the
person in the StartingPosition starts the game
*)
let start (position:StartingPosition) = 
  {
    Player = position
    Board = (4,4,4,4,4,4,4,4,4,4,4,4);
    Score = (0,0);
    State = match position with 
            | North -> NorthTurn
            | South -> SouthTurn
  }


(*
Score: accepts a Board and gives back a tuple of (southScore , northScore)
*)
let score game = 
  game.Score

(*
gameState: accepts a Board and gives back a string that tells us about the
state of the game. Valid strings are “South’s turn”, “North’s turn”, “Game ended in a
draw”, “South won”, and “North won”.
*)
let gameState game = 
  match game.State with 
  | Draw -> "Game ended in a draw"
  | NorthWon -> "North won"
  | SouthWon -> "South won"
  | NorthTurn -> "North's turn"
  | SouthTurn -> "South's turn"

//impure output: 12 lines (all inclusive)
let outputGame game = //function that takes in a game and prints out the Board and scores
  let (a,b,c,d,e,f,a',b',c',d',e',f') = game.Board
  let (s,n) = game.Score
  System.Console.Clear()
  printfn "\n%s\n                           " (gameState game)
  printfn "       {North score}             " 
  printfn "          |~~%i~~|\n             " n
  printfn "  [%i]-[%i]-[%i]-[%i]-[%i]-[%i]\n" f' e' d' c' b' a'  //start bottom left to move in counter-clockwise direction
  printfn "  [%i]-[%i]-[%i]-[%i]-[%i]-[%i]\n" a b c d e f 
  printfn "          |~~%i~~|               " s
  printfn "       {South score}\n           "
  ()

//impure input: 9 lines (all inclusive)
let getUserInput game=
  let rec getConsoleInput () = 
    printf "Please enter a house to move: South(1-6) North(12-7):   "
    let retry () = printfn "Invalid selection, try again" |> getConsoleInput
    let input = System.Console.ReadLine()
    match input with
    | ("1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"|"10"|"11"|"12") -> int input
    | _ -> retry ()
  getConsoleInput()

[<EntryPoint>]
let main _ =

  let startgame = (start South) //choose who starts
  outputGame startgame
  let rec playGame input game =
    match game.State with 
    | Draw | NorthWon | SouthWon -> gameState game
    | _ -> let game = useHouse input game
           outputGame game
           playGame (getUserInput game) game

  let final = playGame (getUserInput startgame) startgame

  0 // return an integer exit code
