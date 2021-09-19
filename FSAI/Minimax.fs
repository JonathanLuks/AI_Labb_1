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

    let applyGetValidMoves operation (board: byte[,]) (tile: byte) =
        let result:ResizeArray<int * int> = operation board tile
        let FS_List = Seq.toList result
        FS_List

    let applyOtherTile operation (tile: byte) =
        let result:byte = operation tile
        result

    let applyMakeMove operation (board: byte[,]) (move: int * int) (tile: byte) =
        let result = operation board move tile
        result

    let rec miniMaxAlphaBeta board depth a b tile isMaxPlayer getValidMoves otherTile makeMove = 
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

        let validMoves = getValidMoves board tile

        let contains =
            if List.isEmpty validMoves then
                let newTile = otherTile tile
                let newIsMaxPlayer = not isMaxPlayer
                miniMaxAlphaBeta board depth a b newTile newIsMaxPlayer getValidMoves otherTile makeMove
        
            else
                let rec loop n =
                    if b => a then
                    
                //for move in validMoves do
                
                        let childBoard:byte[,] = board
                        makeMove childBoard move tile
                        let newDepth = depth - 1
                        let newIsMaxPlayer = not isMaxPlayer
                        let newTile = otherTile tile
                        let nodeScore = miniMaxAlphaBeta childBoard newDepth a b newTile newIsMaxPlayer getValidMoves otherTile makeMove

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
        // else return miniMaxAlphaBeta board depth a b OtherTile !isMaxPlayer


        bestScore


    