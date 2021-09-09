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
        let bestScore = 0
        //if depth = 0 then
            //eval(board)
        if isMaxPlayer then
            bestScore = Max(bestScore, b)
        else
            bestScore = Min(bestScore, a)
        if b <= a then
            break
            

    