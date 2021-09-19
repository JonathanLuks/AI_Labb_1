namespace FSAI
open System

module Minimax =
    let Empty = 0uy
    let White = 1uy
    let Black = 2uy
    let Valid = 3uy
    let Tie = 4uy

    let getScore (board:byte[,]) tile =
        let mutable score = 0
        for cell in board do
            if cell = tile then 
                score <- score + 1
        score

    let countCorners (board:byte[,]) tile =
        let mutable corners = 0

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
        let blackScore = getScore board Black 
        let whiteScore = getScore board White
        let blackMobility:ResizeArray<Tuple<int, int>> = getValidMoves board Black 
        let whiteMobility:ResizeArray<Tuple<int, int>> = getValidMoves board White
        
        let stopVariable =
            if blackScore = 0 then
                -200000
            else if whiteScore = 0 then
                200000
            else 
                0

        if stopVariable < 0 || stopVariable > 0 then
            stopVariable
        else
            if (blackScore + whiteScore) = 64 || (blackMobility.Count + whiteMobility.Count) = 0 then
                if Black < byte whiteScore then
                    (-100000 - whiteScore + blackScore)
                else if blackScore > whiteScore then
                    (100000 + blackScore - whiteScore)
                else
                    0
            else
                if (blackScore + whiteScore) > 55 then
                    (blackScore - whiteScore)
                else
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

    let rec Loop validMoves bestScore isMaxPlayer a b tile board depth miniMaxAlphaBeta getValidMoves makeMove otherTile getWinner= 
        match validMoves with
        | x::xs ->
            let childBoard:byte[,] = board
            makeMove childBoard x tile

            let newDepth = depth - 1
            let newIsMaxPlayer = not isMaxPlayer
            let newTile = otherTile tile
            let nodeScore = miniMaxAlphaBeta childBoard newDepth a b newTile newIsMaxPlayer getValidMoves makeMove otherTile getWinner
        
            if isMaxPlayer then
                let newBestScore = Max bestScore nodeScore
                let newA = max newBestScore a

                if newA > b then
                    newBestScore
                else
                    Loop xs newBestScore isMaxPlayer newA b tile board depth miniMaxAlphaBeta getValidMoves makeMove otherTile getWinner
            else
                let newBestScore = Min bestScore nodeScore
                let newB = Min newBestScore b

                if newB < a then
                    newBestScore
                else
                    Loop xs newBestScore isMaxPlayer a newB tile board depth miniMaxAlphaBeta getValidMoves makeMove otherTile getWinner
        | [] -> 
            bestScore

    let rec miniMaxAlphaBeta (board:byte[,]) depth a b (tile:byte) isMaxPlayer getValidMoves makeMove otherTile getWinner= 
        let validMoves:ResizeArray<Tuple<int, int>> = getValidMoves board tile
        if depth = 0 || (getWinner board <> Empty) then
            eval board getValidMoves
        else
            // There is no int.MaxValue/MinValue in F# so we'll have to do it manually.
            let bestScore = 
                if isMaxPlayer then
                    -2147483648
                else
                    2147483647

            if validMoves.Count > 0 then
                Loop (Seq.toList validMoves)  bestScore isMaxPlayer a b tile board depth miniMaxAlphaBeta getValidMoves makeMove otherTile getWinner
                
            else
                let newTile = otherTile tile
                let newIsMaxPlayer = not isMaxPlayer
                miniMaxAlphaBeta board depth a b newTile newIsMaxPlayer getValidMoves makeMove otherTile getWinner