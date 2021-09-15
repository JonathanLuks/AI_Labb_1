namespace FSAI
open System

module Minimax =
    let printHello message = 
        $"Hello, world {message}"

    type Class1() = 
        member this.X = "F#"

    let eval board = 
        board

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

    let rec miniMaxAlphaBeta board depth a b tile isMaxPlayer getValidMoves MakeMove OtherTile = 
        //if depth = 0 then
            //eval(board)
        // There is not int.MaxValue/MinValue in F# so we'll have to do it manually.
        let maxValue = 2147483647
        let minValue = -2147483648

        let bestScore = 
            if isMaxPlayer then
                minValue
            else
                maxValue

        let validMoves = getValidMoves board tile
        if validMoves.Count > 0 then
            for move in validMoves do
                let childBoard:byte[,] = board
                MakeMove childBoard move tile

                let newDepth = depth - 1
                let newIsMaxPlayer = not isMaxPlayer
                let newTile = OtherTile tile
                let nodeScore = miniMaxAlphaBeta childBoard newDepth a b newTile newIsMaxPlayer getValidMoves MakeMove
        
                let newBestScore =
                    if isMaxPlayer then
                        let newA = Max bestScore a
                        Max bestScore nodeScore
                    else
                        let newB = Min bestScore b
                        Min bestScore nodeScore
                
        // else return miniMaxAlphaBeta board depth a b OtherTile !isMaxPlayer

        bestScore


    