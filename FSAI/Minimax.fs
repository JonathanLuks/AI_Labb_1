namespace FSAI

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

    let applyOperation operation (board:byte[,]) (tile:byte) =
        let result:ResizeArray<int * int> = operation board tile
        result


    let rec miniMaxAlphaBeta board depth a b tile isMaxPlayer = 
        //if depth = 0 then
            //eval(board)

        // There is no int.MaxValue/MinValue in F# so we'll have to do it manually.
        let maxValue = 2147483647
        let minValue = -2147483648

        let bestScore = 
            if isMaxPlayer then
                minValue
            else
                maxValue

        // todo: Get list of Valid Moves
        // if (validMoves.Count > 0)

        let childBoard:byte[,] = board
        // todo: MakeMove, OtherTile
        let newDepth = depth - 1
        let newIsMaxPlayer = not isMaxPlayer
        let nodeScore = miniMaxAlphaBeta childBoard newDepth a b tile newIsMaxPlayer
        

        let newBestScore =
            if isMaxPlayer then
                let newA = Max bestScore a
                Max bestScore nodeScore
            else
                let newB = Min bestScore b
                Min bestScore nodeScore

        
        // else return miniMaxAlphaBeta board depth a b OtherTile !isMaxPlayer


        bestScore


    