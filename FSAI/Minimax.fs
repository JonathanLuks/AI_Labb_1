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

    let miniMaxAlphaBeta board depth a b tile isMaxPlayer = 
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

        // todo: Get list of Valid Moves 



        let move : (int * int) = (0, 0)
        move


    