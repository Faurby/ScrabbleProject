module internal FParsecLight

    open FParsec

    type ParserResult<'a> = ParserResult<'a, unit>

    module TextParser =

        type Parser<'a> = Parser<'a, unit>

        val satisfy : (char -> bool) -> Parser<char>

        val (<?>)   : Parser<'a> -> string -> Parser<'a>

        val (.>>.)  : Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
        val (.>>)   : Parser<'a> -> Parser<'b> -> Parser<'a>
        val (>>.)   : Parser<'a> -> Parser<'b> -> Parser<'b>

        val (|>>)   : Parser<'a> -> ('a -> 'b) -> Parser<'b>

        val (<|>)   : Parser<'a> -> Parser<'a> -> Parser<'a>
        val choice  : seq<Parser<'a>> -> Parser<'a>

        val many    : Parser<'a> -> Parser<'a list>
        val many1   : Parser<'a> -> Parser<'a list>

        val opt     : Parser<'a> -> Parser<'a option>
        val between : Parser<'a> -> Parser<'b> -> Parser<'c> -> Parser<'c>

        val sepBy   : Parser<'a> -> Parser<'b> -> Parser<'a list>
        val sepBy1  : Parser<'a> -> Parser<'b> -> Parser<'a list>

        val anyChar     : Parser<char>
        val asciiLetter : Parser<char>
        val digit       : Parser<char>

        val pchar   : char -> Parser<char>
        val pstring : string -> Parser<string>
        val pint32  : Parser<int>

        val createParserForwardedToRef : unit -> Parser<'a> * Parser<'a> ref

        val run : Parser<'a> -> string -> ParserResult<'a>

        val getSuccess : ParserResult<'a> -> 'a