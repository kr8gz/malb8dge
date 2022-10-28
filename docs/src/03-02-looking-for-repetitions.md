# Looking for repetitions
Here's our progress so far:
```
;"Guess the number!"

number = 1 ?\ 100

? {
    *_("Your guess: ") > number ? {
        ;"Too high!"
    } ! & < number ? {
        ;"Too low!"
    } ! {
        %%"You guessed the number!"
    }
}
```
As you might have noticed, our program now mostly consists of only text!
We can immediately tell that the parts `" the number!"` and `"Too "` each appear twice.
The simplest solution to removing the second `" the number!"` is to just assign it to a new variable using the method we saw in the previous chapter:
```
;"Guess" + (t = " the number!")

...

%%"You guessed" + t
```
This looks like quite a lot of extra symbols... But wait - the `;` operator can actually print more than just a single value!
By creating a list instead of adding the strings together, we can get the same outcome while also removing the brackets, as shown in this code block:
```
;"Guess", t = " the number!"
```
> **Note:** Unfortunately, malb8dge only allows one value to be stored in the global variable at a time.
> This means that we cannot use the global variable again, since we've already used it to replace the variable that stores the player's input.
> You will have to get creative to find a way that will remove as many characters as possible using this method!

Great! Now let's move on to the next part.
To eliminate the duplicate `"Too "`, we will actually use a different method.
The last block that prints the winning message exits the game immediately, which means that all the other branches print `"Too "` plus another word.
We can use this to print another list, with the first part being `"Too "` and the second one being the if expression:
```
;"Too ", *_("Your guess: ") > number ? {
    "high!"
} ! & < number ? {
    "low!"
} ! {
    ### if the program exits here, the outer print statement will not run
    %%"You guessed" + t
}
```
> **Note:** The `###` syntax starts a comment which continues until the end of the line.
> Everything inside a comment will *not* be treated as code!
> Comments can be used to help the reader understand what your code is doing, like in the block above.

There is one last shortcut that will be helpful in this chapter: the `/` operator.
It works like the `;` operator, but it will add spaces between the elements of the list we give it.
Using this trick we can also remove the space character from `"Too "`:
```
/"Too", *_("Your guess: ") > number ? {
    "high!"
} ! & < number ? {
    "low!"
} ! {
    %%"You guessed" + t
}
```
That's about as far as we can go compacting the logic part.
Here's a quick overview of what we've created:
```
;"Guess", t = " the number!"

number = 1 ?\ 100

? {
    /"Too", *_("Your guess: ") > number ? {
        "high!"
    } ! & < number ? {
        "low!"
    } ! {
        %%"You guessed" + t
    }
}
```
Now comes the fun part - in the next chapter, we will turn this confusing mess of symbols into an even worse confusing mess of symbols by removing every single character that doesn't contribute to the program's function!