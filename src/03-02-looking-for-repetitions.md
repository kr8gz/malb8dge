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
As you might have noticed, most of our program now consists of only text!

```
/"Guess",t="the number!"n=1?\100;?/"Too",*_("Your guess: ")<n?"low!"!&>n?"high!"!%%"You guessed {t
```
yay