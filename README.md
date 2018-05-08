# xc-wumpus-prolog-wsi
Wumpus implementation with prolog by wsi

To run the code, first open swipl and load the project.

```bash

$ swipl main.pl 

```

Then, run the code.

```prolog

?- run.

```

Beware, this is not a fully function implementation of the world prolog. Even so, it uses successor state axioms to infer pits wumpus, and safe locations.

The movement of the hunter is also preety well designed, however his heuristics are not quite finished.

