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
                
    let getWinner (board:byte[,]) getScore getValidMoves = 
        let blackScore = getScore board 2
        let whiteScore = getScore board 1
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
            

    let rec miniMaxAlphaBeta (board:byte[,]) (depth:int) (a:int) (b:int) (tile:byte) (isMaxPlayer:bool) getValidMoves otherTile makeMove:int = 
        
        //let result = getWinner board getScore getValidMoves

        // There is no int.MaxValue/MinValue in F# so we'll have to do it manually.
        let maxValue = 2147483647
        let minValue = -2147483648

        let bestScore = 
            if isMaxPlayer then
                minValue
            else
                maxValue
               
        let validMoves:ResizeArray<Tuple<int, int>> = getValidMoves board tile
        
        let contains:int =
            if validMoves.Count <= 0 then
                miniMaxAlphaBeta board depth a b (otherTile tile) (not isMaxPlayer) getValidMoves otherTile makeMove
        
            else
                let rec loop n =
                    if b >= a then
                        let childBoard:byte[,] = board

                        makeMove childBoard validMoves.[n] tile

                        let nodeScore = miniMaxAlphaBeta childBoard (depth - 1) a b (otherTile tile) (not isMaxPlayer) getValidMoves otherTile makeMove

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

        bestScore


    