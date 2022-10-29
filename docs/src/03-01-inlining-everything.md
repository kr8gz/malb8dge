# Inlining *everything*
Let's take another look at our finished program:
```
;"Guess the number!"

number = 100?\

? {
    guess = _("Your guess: ")

    guess > number ? {
        ;"Too high!"
    } ! guess < number ? {
        ;"Too low!"
    } ! {
        ;"You guessed the number!"
        %
    }
}
```
We could easily remove almost half of the characters by deleting unnecessary spaces and newlines, but that would make the code pretty unreadable very fast - so we'll do that later.
What we can do instead is trying to compact the parts inside of the curly brackets to single statements, so that we can remove the brackets entirely.

First, let's look at this part:
```
} ! {
    ;"You guessed the number!"
    %
}
```
malb8dge has a shortcut for exiting a program and also optionally printing a message: `%%message`.
This means that we can replace the last block with a simple exit statement like so:
```
} ! {
    %%"You guessed the number!"
}
```
At the moment this doesn't look much shorter, but it will be once we remove the curly brackets!

The next thing we could do is compact everything that is inside of the loop into one statement.
We can achieve this by inlining the `guess` variable, i.e. giving `guess` its value inside the if expression itself, since variable assignments in malb8dge also return the value that is being assigned.
The code would look something like this:
```
(guess = _("Your guess: ")) > number ? {
    ;"Too high!"
} ! guess < number ? {
    ;"Too low!"
} ! {
    %%"You guessed the number!"
}
```
The brackets around the inline assignment are important, because otherwise `guess` would get the value of the entire if expression.

But of course, this can be shortened too. Using the anonymous global variable, we can even eliminate an entire variable name!
```
*_("Your guess: ") > number ? {
    ;"Too high!"
} ! & < number ? {
    ;"Too low!"
} ! {
    %%"You guessed the number!"
}
```
As you can see, the assignment has been replaced by a `*`, and the variable has turned into a `&`.

We cannot do the same thing to the `number` variable though, since we want the secret number to stay the same during the entire loop, so this is the best we can do!

Before we finally turn this program into an unreadable mess, we'll also look at how we can remove a few other repetitions, specifically the strings!