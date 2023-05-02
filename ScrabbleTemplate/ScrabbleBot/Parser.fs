module internal Parser

    open Eval
    open StateMonad
    open ScrabbleUtil
    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    //open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let spaceParenthesise p = spaces >*>. (parenthesise p) .>*> spaces
    let curlyBrackets p = pchar '{' >*>. p .>*> pchar '}'

    let charListToStr (a: char list) = System.String.Concat(a)

    let pid = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> fun (a, b) -> charListToStr(a::b)

    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul ((N -1), a)) <?> "Neg"
    let VParse = pid |>> V <?> "V"

    let AexpParse = TermParse 

    let CParse, cref = createParserForwardedToRef<cExp>()

    let charParse = between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "C"
    let toUppperParse = unop pToUpper (parenthesise CParse) |>> ToUpper <?> "ToUpper"
    let toLowerParse = unop pToLower (parenthesise CParse) |>> ToLower <?> "ToLower"
    let intToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"
    let charValueParse = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CV"
    do cref := choice [charValueParse; intToCharParse; toUppperParse; toLowerParse; charParse]

    let CharToIntParser = unop pCharToInt (parenthesise CParse) |>> CharToInt <?> "V"
    do aref := choice [CharToIntParser; NegParse; PVParse; VParse; NParse; ParParse]
    
    let CexpParse = CParse

    let BTerm, btref = createParserForwardedToRef<bExp>()
    let BProd, bpref = createParserForwardedToRef<bExp>()
    let BAtom, baref = createParserForwardedToRef<bExp>()

    let andParse = binop (pstring "/\\") BProd BTerm |>> Conj <?> "Conj"
    let orParse = binop (pstring "\\/") BProd BTerm |>> (fun (x, y) -> Not (Conj (Not x, Not y))) <?> "Conj"

    do btref := choice [andParse; orParse; BProd]

    let equalParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "AEq"
    let notEqual = binop (pstring "<>") AexpParse AexpParse |>> (fun (x, y) -> x .<>. y) <?> "Not Equal"
    let lessThanParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "ALt"
    let biggerThanOrEqualParse = binop (pstring ">=") AexpParse AexpParse |>> (fun (x, y) -> x .>=. y) <?> "BiggerOrEqual"
    let biggerThan = binop (pchar '>') AexpParse AexpParse |>> (fun (x,y) -> x .>. y) <?> "Great than"
    let lessOrEqual = binop (pstring "<=") AexpParse AexpParse |>> (fun (x,y) -> x .<=. y) <?> "lessOrEqual"
    do bpref := choice [equalParse; notEqual; lessThanParse; lessOrEqual; biggerThan; biggerThanOrEqualParse; BAtom]

    let trueParse = pTrue |>> (fun _ -> TT) <?> "True"
    let falseParse = pFalse |>> (fun _ -> FF) <?> "False"
    let notParse = unop (pchar '~') BAtom |>> (fun x -> Not x) <?> "Not"
    let isLetterPrase = unop (pIsLetter) CexpParse |>> IsLetter <?> "IsLetter"
    let isVowel = unop (pIsVowel) CexpParse |>> IsVowel <?> "IsVowel"
    let isDigit = unop (pIsDigit) CexpParse |>> IsDigit <?> "IsDigit"
    let parParse = parenthesise BTerm
    do baref := choice [notParse; isLetterPrase; isVowel; isDigit; trueParse; falseParse; parParse]

    let BexpParse = BTerm

    let SFirst, sFirstRef = createParserForwardedToRef<stm>()
    let SSecond, sSecondRef = createParserForwardedToRef<stm>()
    let cbParse = curlyBrackets SFirst <?> "CurlyBrackets"
    
    let seqParse = binop (pchar ';') SSecond SFirst |>> Seq <?> "Seq"
    let iteParse = unop pif (parenthesise BTerm) .>*>. unop pthen cbParse .>*>. unop pelse SSecond |>> (fun ((bool, ifTrue), ifFalse) -> ITE (bool,ifTrue,ifFalse)) <?> "If-then-else"
    let ifParse = unop pif (parenthesise BTerm) .>*>. unop pthen cbParse |>> (fun (bool, ifTrue) -> ITE (bool, ifTrue, Skip)) <?> "If-then"
    let whileParse = unop pwhile (parenthesise BTerm) .>*>. unop pdo cbParse |>> While <?> "While-do"
    let assignParse = binop (pstring ":=") pid TermParse |>> Ass <?> "Assign"
    let declareParse = pdeclare >*>. pid |>> Declare <?> "Declare"
    
    
    do sFirstRef := choice [seqParse; SSecond]
    do sSecondRef := choice [assignParse; declareParse;iteParse;ifParse;cbParse;whileParse]

    let stmntParse = SFirst
    

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>

    type squareFun = word -> int -> int -> Result<int, Error>

    type word   = (char * int) list
    type square = Map<int, squareFun>
    let parseSquareProg (sqp:Map<int,string>) = sqp |> Map.map (fun _ p -> (stmntToSquareFun (getSuccess (run stmntParse p))))
   
    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    
    let parseBoardProg (s:string) (sqs:Map<int, square>) : boardFun2 =
        //printf "Inside parseboardProg: %A \n" sqs
        stmntToBoardFun (getSuccess (run stmntParse s)) sqs

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    let mkBoard (bp : boardProg) =
            let squaresMap = bp.squares
            let squares = Map.map (fun _ squareProg -> parseSquareProg squareProg) squaresMap
            let defaultSquare = Map.find bp.usedSquare squaresMap
            {
                center = bp.center
                defaultSquare = parseSquareProg defaultSquare
                squares = parseBoardProg bp.prog squares
            }
