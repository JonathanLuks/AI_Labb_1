namespace FSAI
open System

module Minimax =
    let Empty = 0uy
    let White = 1uy
    let Black = 2uy
    let Valid = 3uy
    let Tie = 4uy

    (*
        Returns the opposite color of the current tile.
    *)
    let otherTile (tile:byte) =
        if tile = Black then
            White
        else if tile = White then
            Black
        else 
            raise (System.ArgumentException("tile must have value 1 or 2"))

    (*
        Returns the count of the specified tile on the board.
    *)
    let getScore (board:byte[,]) tile =
        let mutable score = 0
        for cell in board do
            if cell = tile then
                score <- score + 1
        score

    (*
        Counts how many of the corners are taken by the specified tile color and returns how many.
    *)
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
    
    // Evaluates a score using the Black/White score, the amount of valid moves left & how many corners each color has taken.
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
            evaluation <- (blackScore - whiteScore) + ((blackMobility.Count - whiteMobility.Count)*10) + ((countCorners board Black - countCorners board White)*100)
        evaluation

 
    (*
        Max-functiion. Takes two values as input and returns the largest one.
    *)
    let Max x y = 
        if x > y then
            x
        else
            y

    (*
        Min-functiion. Takes two values as input and returns the smallest one.
    *)
    let Min x y = 
        if y > x then
            x
        else
            y

    (*
        Iterates through all the possible moves to calculate and return the mini-max value with alpha-beta pruning.
    *)
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

                // Alpha-beta pruning is done here
                if b <= newA then
                    newBestScore
                else
                    Loop xs newBestScore isMaxPlayer newA b tile board depth miniMaxAlphaBeta getValidMoves makeMove getWinner
            else
                let newBestScore = Min bestScore nodeScore
                let newB = Min newBestScore b

                // Alpha-beta pruning is done here
                if  a <= newB then
                    newBestScore
                else
                    Loop xs newBestScore isMaxPlayer a newB tile board depth miniMaxAlphaBeta getValidMoves makeMove getWinner 
        | [] -> 
            // Return if validMoves is empty.
            bestScore

    (*
        MiniMaxAlphaBeta function. Takes multiple parameters including three functions converted from C#.
    *)
    let rec miniMaxAlphaBeta (board:byte[,]) (depth:int) (a:int) (b:int) (tile:byte) (isMaxPlayer:bool) getValidMoves makeMove getWinner = 
        // Returns C#-list of valid moves on the board. Inputs current board and current player (tile-color).
        let validMoves:ResizeArray<Tuple<int, int>> = getValidMoves board tile

        // Manual function of int.Min/Max. Returns the biggest/smallest int32 value.
        let bestScore = 
            if isMaxPlayer then
                -2147483648
            else
                2147483647

        // Checks if game is over or the specified depth has been reached.
        if depth = 0 || (getWinner board <> Empty) then
            // Returns score
            eval board getValidMoves
        else
            if validMoves.Count > 0 then
                // Recursive loop for the AI.
                Loop (Seq.toList validMoves) bestScore isMaxPlayer a b tile board depth miniMaxAlphaBeta getValidMoves makeMove getWinner  
            else
                let newTile = otherTile tile
                let newIsMaxPlayer = not isMaxPlayer
                miniMaxAlphaBeta board depth a b newTile newIsMaxPlayer getValidMoves makeMove getWinner
 