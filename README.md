# HIRC-bot

Tiny (for now) Haskell IRC bot.

Its key functionalities are :

* Auto "PAN !" with case insensitive detection of "coin"
* Repeat whatever is said begining with "!id "
* Quit whent told to with "!quit"

The main goal of this bot is educational, I'm triying to understand Monads :)

You can launch the IRC bot directly with Docker with the command :

~~~bash
docker run -d taeradan/hirc-bot
~~~
