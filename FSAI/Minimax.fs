namespace FSAI

open System

module Minimax =
    let printHello message = 
        $"Hello, world {message}"

    type Class1() = 
        member this.X = "F#"

    let Tie = 4uy
    let Valid = 3uy
    let Black = 2uy
    let White = 1uy
    let Empty = 0uy

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
                
    let getWinner (board:byte[,]) getScore getValidMoves = 
        let blackScore = getScore board 2uy
        let whiteScore = getScore board 1uy
        let blackMoves:ResizeArray<Tuple<int, int>> = getValidMoves board Black
        let whiteMoves:ResizeArray<Tuple<int, int>> = getValidMoves board White


        if blackScore = 0 || whiteScore = 0 || blackScore + whiteScore = 64 then
            if blackScore > whiteScore then
                Black
            elif whiteScore > blackScore then
                White
            else
                Tie
        else
            Empty
            
    let evaluation board getValidMoves getScore =
        let blackScore = getScore board Black
        let whiteScore = getScore board White
        let blackMobility:ResizeArray<Tuple<int, int>> = getValidMoves board Black
        let whiteMobility:ResizeArray<Tuple<int, int>> = getValidMoves board White
        let mutable eval = 0

        if blackScore = 0 then
            -200000
        elif whiteScore = 0 then
            200000
        else
            if blackScore + whiteScore = 64 || (blackMobility.Count + whiteMobility.Count) = 0 then
                if blackScore < whiteScore then
                    -100000 - whiteScore + blackScore
                elif blackScore > whiteScore then
                    100000 + blackScore - whiteScore
                else
                    0
            else
                0

        //eval <- blackScore - whiteScore

            



    let rec miniMaxAlphaBeta (board:byte[,]) (depth:int) (a:int) (b:int) (tile:byte) (isMaxPlayer:bool) getValidMoves otherTile makeMove getScore = 
        
        if depth = 0 || (getWinner board getScore getValidMoves) <> Empty then
            evaluation board getValidMoves getScore

        else
            // There is no int.MaxValue/MinValue in F# so we'll have to do it manually.
            let maxValue = 2147483647
            let minValue = -2147483648

            let bestScore = 
                if isMaxPlayer then
                    minValue
                else
                    maxValue
               
            let validMoves:ResizeArray<Tuple<int, int>> = getValidMoves board tile
        
            if validMoves.Count <= 0 then
                miniMaxAlphaBeta board depth a b (otherTile tile) (not isMaxPlayer) getValidMoves otherTile makeMove getScore
        
            else
                let rec loop n =
                    if b >= a then
                        let childBoard:byte[,] = board

                        makeMove childBoard validMoves.[n] tile

                        let nodeScore = miniMaxAlphaBeta childBoard (depth - 1) a b (otherTile tile) (not isMaxPlayer) getValidMoves otherTile makeMove getScore

                        let newBestScore =
                            if isMaxPlayer then
                                let newA = Max bestScore a
                                Max bestScore nodeScore
                            else
                                let newB = Min bestScore b
                                Min bestScore nodeScore

                        loop (n + 1)
                    else
                        null
                    
                0

    