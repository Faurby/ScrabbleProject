# ScrabbleProject

Created by:

- Lasse Faurby Klausen (lakl@itu.dk)
- Anton Bertelsen (adbe@itu.dk)
- Frederik Rothe (frot@itu.dk)

### Multi-player and dictionary

Yes. The game can run multiple instances of Srabble bots in the same game without conflicts. We have tested three versions of our own bot and it seems to work well.

### Parsing boards playing on all boards

No.

### Parallelism

Yes. We use the mailbox principle to parallelize the search for the longest playable word in our trie.

### Respect the timeout flag

Yes. We respect the timeout flag and return the current longest word that has been found before we timeout.
