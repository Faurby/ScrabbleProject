module internal MoveFinder

    open MultiSet
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
            | Some value -> MultiSet.toList (MultiSet.addSingle (fst value) MultiSet.empty)
            | None _ -> MultiSet.toList (multisetToChar hand)
        
        //debugPrint (sprintf "In getting longest word: \n Word so far: %A\n hand: %A\n\n" accCurrentString hand)
        
        // Fold over our world building blocks
        List.fold (fun (longestWordSoFar:string) letter ->
            
            // Get The string we are looking at now within List.fold
            let currentString = (accCurrentString + (string) letter)
            
            // Get child node dictionary
            let childNode = ScrabbleUtil.Dictionary.step letter dict
          
            // Check child node to see if we have hit the end of a word
            match childNode with
            | Some (currentStringIsWord, nodeDict) ->               
                // If the letter does not already exist on the board we remove it from our hand
                let newHand = match existingLetter with
                | Some _ -> hand
                | None _ -> (MultiSet.removeSingle (charToUint letter) hand)
                
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
    