// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "not implemented"
    let pPointValue = pstring "not implemented"

    let pCharToInt  = pstring "not implemented"
    let pToUpper    = pstring "not implemented"
    let pToLower    = pstring "not implemented"
    let pCharValue  = pstring "not implemented"

    let pTrue       = pstring "not implemented"
    let pFalse      = pstring "not implemented"
    let pIsDigit    = pstring "not implemented"
    let pIsVowel    = pstring "not implemented"
    let pIsLetter   = pstring "not implemented"

    let pif       = pstring "not implemented"
    let pthen     = pstring "not implemented"
    let pelse     = pstring "not implemented"
    let pwhile    = pstring "not implemented"
    let pdo       = pstring "not implemented"
    let pdeclare  = pstring "not implemented"

    let whitespaceChar = pstring "not implemented"
    let pletter        = pstring "not implemented"
    let palphanumeric  = pstring "not implemented"

    let spaces         = pstring "not implemented"
    let spaces1        = pstring "not implemented"

    let (.>*>.) _ _ = failwith "not implemented"
    let (.>*>) _ _  = failwith "not implemented"
    let (>*>.) _ _  = failwith "not implemented"

    let parenthesise _ = failwith "not implemented"

    let pid = pstring "not implemented"

    
    let unop _  = failwith "not implemented"
    let binop _  = failwith "not implemented"

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let AexpParse = TermParse 

    let CexpParse = pstring "not implemented"

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) : board = failwith "not implemented"

