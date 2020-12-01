module BowlingKata

type Game = int list

type FrameResult = 
    | Strike of int
    | Spare of int
    | Open of int
    | Bonus of int
    | InvalidFrame 

let newGame() : Game = []

let roll (pins:int) game : Game =
     game @ [pins]

let removeFirstElement elements = 
    List.skip 1 elements

let isStrike pins =
    pins = 10

let isSpare firstScore secondScore =
    firstScore + secondScore = 10

let isOpen firstScore secondScore =
    firstScore + secondScore < 10

let rec addStrikeBonus game = 
    match game with
    | [] -> 0
    | head :: tail -> head + tail.Head

let isLastFrame game =
        List.length game = 1 || List.length game = 2

let isSpareFrame spareFrame = 
    match spareFrame with
    | Spare(_) -> true
    | _ -> false
    
let isStrikeFrame strikeFrame = 
    match strikeFrame with
    | Strike(_) -> true
    | _ -> false

let calculateStrikeScore game scoredFrames =
    if isLastFrame game then
        scoredFrames @ [Strike 10]
    else
        scoredFrames @ [10 + addStrikeBonus game |> Strike]

let calculateSpareScore game scoredFrames =
    if isLastFrame game then
        scoredFrames @ [Spare 10]
    else
        scoredFrames @ [10 + game.[1] |> Spare]
        
let calculateOpenScore firstScore secondScore scoredFrames =
    scoredFrames @ [firstScore + secondScore |> Open]

let frameIsInvalid frame = 
    match frame with
    | InvalidFrame -> true
    | _ -> false

let frameIsValid frame = 
    match frame with
    | InvalidFrame -> false
    | _ -> true

let isValid frames = 
    match frames with
    | [] -> false
    | _ when (isSpareFrame frames.Head || isStrikeFrame frames.Head) && frames.Length = 10 -> false
    | _ when List.exists frameIsInvalid frames || frames.Length < 10 -> false
    | _ when List.length frames = 10 -> true
    | _ when frames.Length = 11 && isSpareFrame frames.[1] || (isStrikeFrame frames.[1] && isStrikeFrame frames.Head |> not) -> true
    | _ when frames.Length = 12 && List.take 3 frames |> List.forall isStrikeFrame -> true
    | _ -> false
   
let rec getFrames (game:Game) : FrameResult list =
    match game with
    | [] -> []
    | _ when game.Head > 10 -> [InvalidFrame]
    | bonusRoundScore when List.length game = 1 && bonusRoundScore.Head <= 10 -> [if isStrike bonusRoundScore.Head then Strike bonusRoundScore.Head else Bonus bonusRoundScore.Head] 
    | head :: tail when isStrike head -> getFrames tail |> calculateStrikeScore tail
    | head :: tail when isSpare head tail.Head -> removeFirstElement tail |> getFrames |> calculateSpareScore tail
    | head :: tail when isOpen head tail.Head ->  removeFirstElement tail |> getFrames |> calculateOpenScore head tail.Head
    | _ -> [InvalidFrame]

let rec frameResultScore frame = 
    match frame with
    | Strike score | Spare score | Open score | Bonus score -> score
    | InvalidFrame -> 0

let score (game:Game) : int option = 
      if game.Length > 21 then
        None
      else
          let frames = getFrames game
          match isValid frames with
          | true -> List.where frameIsValid frames |> List.fold (fun total frame -> total + frameResultScore frame) 0 |> Some
          | false -> None
