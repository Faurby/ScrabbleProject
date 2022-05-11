module internal Utility
    
    open MultiSet

    type coord = (int * int)
    type dir = (int * int)

    // char number to points (borrowed from https://github.com/4lgn/scrabble-bot/blob/master/ScrabbleBot/Helpers.fs)
    let charNumberToPoints (ch: int) = 
            match ch with
            | 0                                             -> 0
            | 1 | 5 | 9 | 12 | 14 | 15 | 18 | 19 | 20 | 21  -> 1
            | 4 | 7                                         -> 2
            | 2 | 3 | 13 | 16                               -> 3
            | 6 | 8 | 22 | 23 | 25                          -> 4
            | 11                                            -> 5
            | 10 | 24                                       -> 8
            | 17 | 26                                       -> 10
            | _                                             -> failwith "Not valid character index"

    let uintToChar id = char(id + 64u)

    // char to uint (borrowed from https://github.com/4lgn/scrabble-bot/blob/master/ScrabbleBot/Helpers.fs)
    let charToUint ch = 
        if (ch = '?') then 0u
        else uint32(System.Char.ToUpper(ch)) - 64u

    // Given a hand, return list of chars
    let handToChar hand =
        MultiSet.toList hand |> List.map uintToChar

    // Convert multiset<uint32> to multiset<char>
    let multisetToChar = MultiSet.map (fun (i:uint) ->
        if i = 0u then
            //Kinda cursed ngl
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray() |> Array.toList
        else
            [uintToChar i]
    ) 

    // Given a word, convert to list of moves
    let wordToMove (word:string) (startingCoord:coord) (direction: (int*int)) (playedLetters: Map<coord, (char * int)>) =
        let aux (word:string) (startingCoord:coord) (direction: (int*int)) (playedLetters: Map<coord, (char * int)>) =
            List.fold (fun (acc:(list<(int*int) * (uint32 * (char * int))> * (int * bool))) char ->
                let (directionRight, directionDown) = direction
                let (x, y) = startingCoord
                let index = (fst (snd acc))
                let charIsWildcard = (snd (snd acc))
                let listOfMoves = (fst acc)

                let coordToPlaceLetter = (x + (index * directionRight), (y + (index * directionDown)))
                let charNumber =
                    if charIsWildcard then
                        0u
                    else
                        charToUint char

                // Print coordtoplaceletter
                //debugPrint (sprintf "CoordToPlaceLetter: %d, %d\n" (fst coordToPlaceLetter) (snd coordToPlaceLetter))

                match playedLetters.TryGetValue coordToPlaceLetter with
                | (true, _) -> (listOfMoves, (index+1,false))
                | (false, _) ->
                    // We store wildcards in word as a question mark followed by the used letter. This means we have to check if we
                    // have hit a question mark and if we have we have to skip it and play a wildcard on the next move
                    if char = '?' then
                        (listOfMoves, (index,true))
                    else
                        (coordToPlaceLetter, (charNumber,(char, charNumberToPoints (int charNumber))))::listOfMoves, (index+1,false)
            ) ([], (0,false)) (word |> Seq.toList)
        fst (aux word startingCoord direction playedLetters)

