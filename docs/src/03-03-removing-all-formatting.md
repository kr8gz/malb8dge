# Removing all formatting
We're going to start by renaming the `number` variable to something shorter:
```
;"Guess", t = " the number!"

n = 100?\

? {
    /"Too", *_("Your guess: ") > n ? {
        "high!"
    } ! & < n ? {
        "low!"
    } ! {
        %%"You guessed" + t
    }
}
```
Then removing the curly brackets:
```
;"Guess", t = " the number!"

n = 100?\

? /"Too", *_("Your guess: ") > n ? "high!" ! & < n ? "low!" ! %%"You guessed" + t
```
Then the spaces:
```
;"Guess",t=" the number!"
n=1?\100
?/"Too",*_("Your guess: ")>n?"high!"!&<n?"low!"!%%"You guessed"+t
```
If we move all the statements into one line, there's another trick we can use.
Because the goal is to remove as many characters as possible, malb8dge doesn't actually require you to separate statements with a special symbol like a semicolon.
You will only have to add semicolons when it's not clear where the statement would end.
For example, let's combine our first two statements: `;"Guess",t=" the number!"n=100?\`.
Since there is no comma between them, this would be invalid syntax - but malb8dge just ignores that and starts a new statement!
Unfortunately we can't do that with the other two statements, because `100?\?...` would be an if expression instead of an infinite loop, which means that we have to add a semicolon:
```
;"Guess",t=" the number!"n=100?\;?/"Too",*_("Your guess: ")>n?"high!"!&<n?"low!"!%%"You guessed"+t
```
Now there's one thing left that we can do! We can write `%%"You guessed"+t` as `%%"You guessed{t}"`.
This looks like we're adding an extra character - but at the end of a program, it is not necessary to close all strings and brackets!
This means that we can omit the `}` and `"`, saving us another character:
```
;"Guess",t=" the number!"n=100?\;?/"Too",*_("Your guess: ")<n?"low!"!&>n?"high!"!%%"You guessed{t
```
Nice! Now our program only contains 97 characters!