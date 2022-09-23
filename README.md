# DODGE.rkt - A 2D dodging game made in Racket

DODGE.rkt is a two-dimensional top-down dosging game, in which one attempts to avoid fast-moving rectangles coming at you from all directions through levels of incresing difficulty!

These rectangles are randomly generated with different velocities and sizes, and their art style attempts to recreate the abstract paintings of Dutch artist Piet Mondriaan.

The game has been designed to demonstrate the endless possibilities available even in simple functional programming languages like Racket.

## To Run
Download zip of files or clone to your own computer.

Open dodgeJB.rkt in DrRacket.

Press 'Run' and then type (start) in the interactions window to play!


## To Play
When the game is launched, you are taken to the Main Menu, from where one can begin playing, by pressing the spacebar.

The player character is controlled with the WASD keys, which can be used both alone and in combination for 8 distinct directions of movement.

If you collide with a rectangle, that's game over and you'll have to start the level again, so watch out!

The game starts at Level 1 by default, but the player can press the number keys 1, 2, 3, and 4 while in the menu to change the level.

Level x multiplies the spawn rate and velocity of the rectangles by x, as well as adding x to the player movement speed.

The first three levels can be completed by surviving for 30 seconds, where the player will be greeted by a win screen and asked if they would like to continue.

The final level is endless, and its difficulty scales with time spent alive, starting from somewhere inbetween the difficulty of 2 and 3.

After beating the timed levels, continue to the endless level to try beat your own high score, which is saved and displayed in the menu.

The game can be paused at any time by pressing 'escape' and resumed again with 'space' or exited with another 'escape'.


## Quick controls reference
WASD to move

Esc to pause and return to menu when paused

Space to start from main menu and resume from pause screen

1, 2, 3, 4 to change difficulty freom the Main Menu
