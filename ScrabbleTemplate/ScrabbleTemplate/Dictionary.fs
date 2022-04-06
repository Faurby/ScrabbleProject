module Dictionary

    type Dict =
        | Leaf of bool //(* empty csDict)
        | Node of bool * System.Collections.Generic.Dictionary<char, Dict> 

    type csDict = System.Collections.Generic.Dictionary<char, Dict> 

    let empty () = Leaf false

    let rec insert (word: string) =
        function
        // Word is done inserting, what to do in trie.
        | Leaf _ when word.Length = 0 -> Leaf true
        | Node (_, csDict) when word.Length = 0 -> Node(true, csDict)

        // Convert leaf to node, and go deeper
        | Leaf b  -> 
            let tmp = csDict ()
            let c = word.[0]
            tmp.[c] <- insert word.[1..] (empty ())
            Node(b, tmp)
        
        // Insert or replace node.
        | Node (b, dic) ->
            let c = word.[0]

            // If char already exists, build in existing dic, otherwise create empty.
            match dic.TryGetValue c with
            | (true, value) ->
                dic.[c] <- insert word.[1..] value
                Node(b, dic)
            | (false, _)    ->
                dic.[c] <- insert word.[1..] (empty())
                Node(b, dic)
    

    let rec lookup (word: string) =
        function
        // We reach end of trie.
        | Leaf b when word.Length = 0 -> b
        | Leaf _ -> false

        // We reach end of word in node.
        | Node (b, _) when word.Length = 0 -> b
        // We reach node and must go deeper (if char is in dic), otherwise return false.
        | Node (b, dic) ->
            match dic.TryGetValue word.[0] with
            | (true, value) -> lookup word.[1..] value
            | (false, _) -> false

    let step (c: char) =
        function
        | Node (_, dic) ->
            match dic.TryGetValue c with
            | (true, value) -> 
                match value with
                | Leaf b -> Some (b, value)
                | Node (b, _) -> Some (b, value)
            | (false, _) -> None
        | Leaf _ -> None