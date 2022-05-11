module internal MoveFinder

    open System.Diagnostics
    open MultiSet
    open ScrabbleUtil
    open Utility
    open ScrabbleUtil.Dictionary

    let rec getLongestWordFirstMove (hand : MultiSet<uint32>) (accCurrentString : string) (dict : Dict) (playedLetters: Map<coord, (char * int)>) (coord:coord) (direction:(int * int)) =

        // First determine what exists at the coordinate of the letter we are writing. If there is already a letter we must weave
        // This into the word we are writing rather than using letters from our hand
        let existingLetter = Map.tryFind coord playedLetters
                
        // This is what we will be building the word with. (What we fold over)
        // If there is not already a letter on the board we use the letters in our hand
        // If there is a letter on the board we use this letter instead of our hand. This lets us
        // weave in existing letters on the board to the word we are constructing
        let wordBuildingBlock =
            match existingLetter with
            | Some value -> MultiSet.toList (MultiSet.addSingle ([fst value]) MultiSet.empty)
            | None _ -> MultiSet.toList (multisetToChar hand)
        
        //debugPrint (sprintf "In getting longest word: \n Word so far: %A\n hand: %A\n\n" accCurrentString hand)
        
        // We fold twice, first over our word building blocks and secondly over the letters in each building block.
        // The reason for this is that wild card pieces have multiple possible letters
        List.fold (fun (longestWordSoFar:string) (lettersInLetterPiece:char list) ->
            // Fold over our world building blocks
            let newPotentialLongestWord =
                List.fold (fun (longestWordSoFar:string) letter ->
                    
                    // Get child node dictionary
                    let childNode = ScrabbleUtil.Dictionary.step letter dict
                                      
                    // Check child node to see if we have hit the end of a word
                    match childNode with
                    | Some (currentStringIsWord, nodeDict) ->
                        
                        // Get The string we are looking at now within List.fold
                        // If we have played a wildcard we prefix the character with a ?
                        // so we are able to see that once we convert the word to a move
                        let currentString = 
                            if lettersInLetterPiece.Length = 1 then
                                // Removing single letter
                                (accCurrentString + (string) letter)
                            else
                                // Removing single wildcard
                                (accCurrentString + "?" + (string) letter)
                        
                        // If the letter does not already exist on the board we remove it from our hand
                        let newHand = match existingLetter with
                        | Some _ -> hand
                        | None _ ->
                            // If the piece has more than one letter in it we assume it is a wildcard to avoid removing the wrong letter.
                            // This could be improved
                            if lettersInLetterPiece.Length = 1 then
                                // Removing single letter
                                (MultiSet.removeSingle (charToUint letter) hand)
                            else
                                // Removing single wildcard
                                (MultiSet.removeSingle 0u hand)
                        
                        // Figure out the coord based on the direction and last coord
                        let newCoord = (fst(coord) + fst(direction), snd(coord) + snd(direction))
                        
                        // Recursively piece together all words
                        let longestWordInBranches = getLongestWordFirstMove newHand currentString (nodeDict)  (playedLetters: Map<coord, (char * int)>) newCoord direction
                        
                        if currentStringIsWord && currentString.Length > longestWordInBranches.Length && currentString.Length > longestWordSoFar.Length then
                            currentString
                        elif longestWordInBranches.Length > longestWordSoFar.Length then
                            longestWordInBranches
                        else
                            longestWordSoFar
                    | None _ -> longestWordSoFar
                ) "" lettersInLetterPiece
            if newPotentialLongestWord.Length > longestWordSoFar.Length then
                newPotentialLongestWord
            else
                longestWordSoFar
        ) "" wordBuildingBlock
    
    let getLongestWordContinuation (hand : MultiSet<uint32>) (accCurrentString: string) (dict: Dict) (playedLetters: Map<coord, (char * int)>) (coord:coord) (direction:(int * int))=
        // Get dict from word so far
        let (wordLength, wordDictSoFar) =
            List.fold (fun (depth:int, dict:Dict) letter ->
                let childNode = ScrabbleUtil.Dictionary.step letter dict
                match childNode with
                | Some (_, nodeDict) ->
                    (depth + 1, nodeDict)
            ) (0,dict) (accCurrentString |> Seq.toList)
        getLongestWordFirstMove hand accCurrentString wordDictSoFar playedLetters ((fst coord) + ((fst direction) * wordLength),(snd coord) + ((snd direction) * wordLength)) direction  
    