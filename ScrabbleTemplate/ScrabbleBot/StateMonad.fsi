module internal StateMonad

    type SM<'a>
    type State

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string

    val mkState : (string * int) list -> (char * int) list -> string list -> State

    val ret    : 'a -> SM<'a>
    val fail   : Error -> SM<'a>
    val (>>=)  : SM<'a> -> ('a -> SM<'b>) -> SM<'b>
    val (>>>=) : SM<unit> -> SM<'a> -> SM<'a>

    val evalSM : State -> SM<'a>  -> Result<'a, Error>

    val push : SM<unit>
    val pop  : SM<unit>

    val lookup  : string -> SM<int>
    val update  : string -> int -> SM<unit>
    val declare : string -> SM<unit>

    val wordLength     : SM<int>
    val characterValue : int -> SM<char>
    val pointValue     : int -> SM<int>
