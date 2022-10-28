# Removing all formatting
We're going to start off by removing all curly braces:
```
;"Guess", t = " the number!"

number = 1 ?\ 100

? /"Too", *_("Your guess: ") > number ? "high!" ! & < number ? "low!" ! %%"You guessed" + t
```

```
;"Guess",t=" the number!"n=1?\100;?/"Too",*_("Your guess: ")<n?"low!"!&>n?"high!"!%%"You guessed{t
```
yay