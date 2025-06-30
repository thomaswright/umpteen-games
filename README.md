# Umpteen Games

https://thomaswright.github.io/umpteen-games/

This is a playing card game engine and game suite.

## Organization & Contribution

"GameBase.res" is the engine. There's an extension of sorts called "Packer.res" for packing-style games (most of the games included).

The game object most often used is "Card.res", but we have a game "Up & Down" similar to [Fortune's Foundation](https://www.zachtronics.com/solitaire-collection/), which uses "Tarot.res" to show this isn't baked in.

You could even use this as a generic drag-and-drop library though the whole thing needs to be cleaned up and pulled apart.

Playing card games share a lot of mechanics, these are listed in "Bases.res", "Boards.res", and "Rules.res", and then combined in each game (with one-off logic as needed).
It is pretty easy to make a new game from these building blocks. Feel free to submit a PR if you want to build some. There's a good listing here https://en.wikipedia.org/wiki/Template:Playing_cards . I have a personal list of over 600!

## Bugs, etc.

Currently "auto-move" and "auto-progress" logic works per designated individual spaces. It would nice to make this work per space type so that you can, say, auto-move to the free-space group.

There's sometimes jank re drag-and-drop. We can't all be perfect.

There's a million other things too of course.
