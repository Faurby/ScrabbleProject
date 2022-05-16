# ScrabbleProject

Created by:

- Lasse Faurby Klausen (lakl@itu.dk)
- Anton Bertelsen (adbe@itu.dk)
- Frederik Rothe (frot@itu.dk)

### Multi-player and dictionary

Yes. The game can run multiple instances of Srabble bots in the same game without conflicts. We have tested combinations of 1 to 4 instances mixing Oxyphenbutazone and our bot.

### Parsing boards playing on all boards

Yes. We are able to parse boards and play on all boards. We can play the first word at the correct coordinate and avoid borders and holes on boards. However, our heuristic for choosing words does not take points into account. Our bot finds the longest word it can play starting at an existing word. This means it creates words that extend
to the right and down. For this reason, the bot performs best on infinite boards and can have difficulties finding wordson random boards if the starting coordinate is near the lower right corner.

### Parallelism

Yes. We use the mailbox principle to parallelize the search for the longest playable word in our trie.

### Respect the timeout flag

Yes. We respect the timeout flag and return the current longest word that has been found before we timeout.

### How to run
cd into ScrabbleTemplate folder

    cd ScrabbleTemplate

run the program

    dotnet run