namespace FSAI
open System

module Minimax =
    let Empty = 0uy
    let White = 1uy
    let Black = 2uy
    let Valid = 3uy
    let Tie = 4uy

    let otherTile (tile:byte) =
        if tile = Black then
            White
        else if tile = White then
            Black
        else 
            raise (System.ArgumentException("tile must have value 1 or 2"))

    let getScore (board:byte[,]) tile =
        let mutable score = 0
        for cell in board do
            if cell = tile then
                score <- score + 1
        score

    let countCorners (board:byte[,]) tile =
        let mutable corners:int = 0

        if board.[0,0] = tile then
            corners <- corners + 1
        if board.[0,7] = tile then
            corners <- corners + 1
        if board.[7,0] = tile then
            corners <- corners + 1
        if board.[7,7] = tile then
            corners <- corners + 1

        corners

    let eval (board:byte[,]) getValidMoves =  
        let mutable evaluation = 0
        let blackScore:int = getScore board Black 
        let whiteScore:int = getScore board White
        let blackMobility:ResizeArray<Tuple<int, int>> = getValidMoves board Black 
        let whiteMobility:ResizeArray<Tuple<int, int>> = getValidMoves board White
        
  
        if blackScore = 0 then
            evaluation <- -200000
        else if whiteScore = 0 then
            evaluation <- 200000

        if (blackScore + whiteScore) = 64 || (blackMobility.Count + whiteMobility.Count) = 0 && evaluation = 0 then
            if Black < byte whiteScore then
                evaluation <- (-100000 - whiteScore + blackScore)
            else if blackScore > whiteScore then
                evaluation <- (100000 + blackScore - whiteScore)
            else
                evaluation <-0
       
        if (blackScore + whiteScore) > 55 && evaluation = 0 then
            evaluation <- (blackScore - whiteScore)
        if evaluation = 0 then
            evaluation <- (blackScore - whiteScore) + ((blackMobility.Count - whiteMobility.Count)*10) + (countCorners board Black - countCorners board White)
        evaluation

 

    let Max x y = 
        if x > y then
            x
        else
            y

    let Min x y = 
        if y > x then
            x
        else
            y

    let rec Loop (validMoves:(int*int)List) (bestScore:int) (isMaxPlayer:bool) (a:int) (b:int) (tile:byte) (board:byte[,]) (depth:int) miniMaxAlphaBeta getValidMoves makeMove getWinner = 
        match validMoves with
        | x::xs ->
            let childBoard:byte[,] = board
            makeMove childBoard x tile

            let newDepth = depth - 1
            let newIsMaxPlayer = not isMaxPlayer
            let newTile = otherTile tile
            let nodeScore = miniMaxAlphaBeta childBoard newDepth a b newTile newIsMaxPlayer getValidMoves makeMove getWinner
        
            if isMaxPlayer then
                let newBestScore = Max bestScore nodeScore
                let newA = Max newBestScore a

                if b <= newA then
                    newBestScore
                else
                    Loop xs newBestScore isMaxPlayer newA b tile board depth miniMaxAlphaBeta getValidMoves makeMove getWinner
            else
                let newBestScore = Min bestScore nodeScore
                let newB = Min newBestScore b

                if  a <= newB then
                    newBestScore
                else
                    Loop xs newBestScore isMaxPlayer a newB tile board depth miniMaxAlphaBeta getValidMoves makeMove getWinner 
        | [] -> 
            bestScore

    let rec miniMaxAlphaBeta (board:byte[,]) (depth:int) (a:int) (b:int) (tile:byte) (isMaxPlayer:bool) getValidMoves makeMove getWinner = 
        let validMoves:ResizeArray<Tuple<int, int>> = getValidMoves board tile
        let bestScore = 
            if isMaxPlayer then
                -2147483648
            else
                2147483647

        if depth = 0 || (getWinner board <> Empty) then
            eval board getValidMoves
        else
            // There is no int.MaxValue/MinValue in F# so we'll have to do it manually

            if validMoves.Count > 0 then
                Loop (Seq.toList validMoves) bestScore isMaxPlayer a b tile board depth miniMaxAlphaBeta getValidMoves makeMove getWinner
                
            else
                let newTile = otherTile tile
                let newIsMaxPlayer = not isMaxPlayer
                miniMaxAlphaBeta board depth a b newTile newIsMaxPlayer getValidMoves makeMove getWinner
 