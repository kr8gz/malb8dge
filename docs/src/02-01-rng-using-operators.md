# RNG using operators

## Introducing malbd8ge's operators
Operators are usually funny mathematical symbols that have strange behaviors.
malb8dge takes that to the maximum by making almost every simple function have its own 1 or 2 symbol combination, each with their own strange behavior for every single type of value.
This means that malb8dge has many, *many* operators.
If you were programming in another language and wanted to get the absolute value of a number, like for example `-3`, you would usually expect there to be a function that looks like `abs(-3)` or `(-3).abs()`.
But guess what - malb8dge has an operator for that instead, so `abs(-3)` would turn into `#-3`. (By the way, `-` is another operator, so we've actually already used two operators in this example!)

Operators come in three different types:
* Before (`-x`)
* After (`x$`)
* Binary (`x + y`)

The position is very important as it changes the entire meaning of the operator!
Take `'x` and `x'` for example - one converts a number to a character and vice versa, while the other rounds a number.

A list of all the operators in malb8dge can be found [here](TODO).

## Generating a random number
For our game, we will generate a number between 1 and 100, including both ends.
Of course malb8dge has an operator for that: `a ?\ b`. It is in the binary position since we need 2 values, one for each end.
(It looks very ugly, but that's not the point of malb8dge. It will only get uglier.)

Create a new file called *guessing_game.mlb8* and paste in the following code:
```
number = 1 ?\ 100
```

This will generate a random number within our specified range and assign it to the variable `number`.

With our first piece of the puzzle in place, we can now move on to the next part: creating a loop and allowing the user to input their guesses.