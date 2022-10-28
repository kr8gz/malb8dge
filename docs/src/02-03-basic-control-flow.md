# Basic control flow
Now that we have both the secret number and the player's guess, we can compare the two and let the program decide what should happen next.

Here are the last two bullet points from the introduction:
* A message will be printed that states whether the guess was too high or too low.
* If the guess is equal to the secret number, a message appears telling the player that they won and the program will stop.

## Conditional code with if expressions
To figure out whether the guess was too high or too low, we'll create an if expression with 2 branches.
An if expression allows you to run a block of code only if a specific condition is met.
In this case we want one branch to run only when the guess was too high, and the other one only when it's too low.
malb8dge avoids words wherever possible, so the syntax looks quite different from what you're probably used to:
```
guess > number ? {

} ! guess < number ? {

}
```
As you may have noticed, the `?` (which in this case stands for "if" as opposed to the infinite loop from [earlier](02-02-loops-and-input.md#creating-a-loop)) comes after the specified condition.
After that comes a block with the code we want to run when that condition is met.
The `!` after the block means "else", which is followed by another block with the code we want to run when the condition is not met.
In the code example above, this else block is actually another if expression with a different condition, which is exactly how you chain if-else expressions in malb8dge.

Now that we have the basic structure, we can use the `;` operator we learned about in [chapter 1](01-02-hello-world.md#hello-world) to output a helpful message.
```
guess > number ? {
    ;"Too high!"
} ! guess < number ? {
    ;"Too low!"
}
```

> **Note:** If you haven't already noticed, we are comparing a string (the input) to a number, which seems very odd.
> How will the comparison work if the input isn't a number?
> Instead of converting the string to a number, the number will be converted to a string, which isn't very useful.
> But if the player inputs an invalid guess, it will never be correct anyway.
> Instead, they might be a bit confused why `hello` is too high of a guess.
> But again, this is not the point of malb8dge.
> If you *do* care about these kinds of things, you're probably better off learning a language like Rust instead.

## Breaking out of a loop
To solve the last bullet point, we will create another else block after the two existing branches.
Inside this block, the player will be told that they have guessed the correct number and the program will be stopped.
For that, we will use a break statement which will *break* out of the infinite loop, and since there is nothing after the loop, the program will end.
The malb8dge equivalent of a break statement is `%`. After adding all of that, you should get this:
```
guess > number ? {
    ;"Too high!"
} ! guess < number ? {
    ;"Too low!"
} ! {
    ;"You guessed the number!"
    %
}
```

If we put these if expressions into our infinite loop and add a few finishing touches, our first llittle malb8dge project is finished!
```
;"Guess the number!"

number = 1 ?\ 100

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

Awesome! You have now learnt about some basic malb8dge syntax.
In the next chapter, we will take a look at how we can compact the code that we've written in this chapter.