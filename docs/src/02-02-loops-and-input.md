# Loops and input

## Creating a loop
There are several different kinds of loops in malb8dge, but for our guessing game we'll only need the simplest one: an infinite loop.
You might know this as a `while true` loop from other programming languages, but malb8dge has a shortcut for that:
```
? {

}
```

All our following instructions will go inside of this loop, i.e. between the curly braces, since we want the player to have infinitely many tries to guess our secret number.

## Getting the player's input
We can get the player's guesses by using `_`, which will wait for the user to input something and return the input as text.
After that, we can assign that value to another variable like so: 
```
guess = _
```

We can also specify an input prompt like this:
```
guess = _("Your guess: ")
```
This will print `Your guess: `&zwnj; before waiting for user input.

Your code should now look something like this:
```
number = 1 ?\ 100

? {
    guess = _("Your guess: ")
}
```

In the next chapter, we will compare the guess to the secret number and give the player some feedback on their guess!