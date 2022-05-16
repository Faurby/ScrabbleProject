// This file is new and hides the implementation details of the parser.

module internal Parser
    
    open ScrabbleUtil
    open StateMonad
    
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    val mkBoard : boardProg -> board