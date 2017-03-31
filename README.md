Handcraft
=========

A simple program prompted by https://www.reddit.com/r/Minecraft/comments/61wjcb/

There is at least one bug that causes missing spaces in the output, and the
HTML parsing module also has a few more bugs which don't currently affect the
program. Both parsing modules should be lifted out to separate libraries.

The list of 2x2 crafting recipies is in handcraft.txt. They are in the order
processed: each section is processed concurrently, recipies within each section
are processed in order.

Building
--------

I recommend using stack because the version of phaser required is not yet
on hackage.
