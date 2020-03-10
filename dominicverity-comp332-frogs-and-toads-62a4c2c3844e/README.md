# Macquarie University, Department of Computing #

## COMP332 Programming Languages 2019 ##

## Solving the Frogs and Toads problem in Scala ##

![10 frogs and 10 toads](frogs10-and-toads10.gif)

### Introduction ###

This project provides (a minimal skeleton for) a Scala application to solve the [frogs and toads puzzle](https://www.cut-the-knot.org/SimpleGames/FrogsAndToads.shtml) and to generate an animation of the solution found. This puzzle was originally discussed in the book [*"Winning Ways for your Mathematical Plays"*](https://www.amazon.com/exec/obidos/ISBN=1568811306) by E.R. Berlekamp, J.H. Conway, R.K. Guy.

The rules of the frogs and toads puzzle are very simple:

* The puzzle takes place on a board consisting of a horizontal row of adjacent *cells*, there are `N + M + 1` cells in all, where `N` and `M` are fixed whole numbers greater than `0`.
* At any time each cell can either be empty, it can contain a single *frog*, or it can contain a single *toad*.
* The puzzle starts with `N` frogs populating the `N` consecutive cells at the left of the board and `M` toads populating the `M` consecutive cells at the right of the board. That, of course, means that there is a single empty cell between these phalanxes of frogs and toads.
* At each move of the puzzle, either a single frog can make a single move or a single toad can make a single move.
* A *frog* can only move from left to right as follows:
  * it can *slide* into an empty cell immediately to its right, or
  * it can *jump* into an empty cell over the head of a **single** toad immediately to its right.
* A *toad* can only move from right to left as follows:
  * it can *slide* into an empty cell immediately to its left, or
  * it can *jump* into an empty cell over the head of a **single** frog immediately to its left.

The aim of the puzzle is to find a sequence of moves (legal slides and jumps) which leave the board with all of the toads on the *left* and all of the frogs on the *right*.

The animated image above illustrates the solution to this problem in the case of 10 frogs, which sit in the cells coloured light green, and 10 toads, which sit in the cells coloured dark green. The arrow indicates the move (slide or jump) made at each step of the solution being animated.

Of course, the numbers of frogs and toads needn't be the same in an instance of the puzzle. For example, here is an image illustrating a solution to the puzzle with 5 frogs and 8 toads:

![5 frogs and 8 toads](frogs5-and-toads8.gif)

These animated images have been generated using the animation facilities in the latest version of the [Doodle Compositional Vector Graphics library](https://github.com/creativescala/doodle).

### Solving this problem ###

The purpose of this project is to construct and animate a solution to the frogs and toads puzzle for given numbers of frogs and toads. It is the case that this puzzle can always be solved for any starting numbers of frogs and toads.

**Note:** There is a subtle difference between writing an application to find a solution to a puzzle, if one exists, and constructing a mathematical proof that there must always exist a solution to that puzzle. These are certainly related activities but luckily for us, as programmers, the former is usually a little easier than the latter. We don't need to prove that the frogs and toads puzzle always has a solution, we just need to search for a solution to a specific instance of the puzzle.

Dom's solution to this problem works as follows:

A `PuzzleState` object describes the positions of the frogs and toads (and the empty cell) at a specific point in time. The `PuzzleState` object with all the frogs on the left and all the toads on the right is called the *initial state*. The `PuzzleState` object with all the toads on the left and all the frogs on the right is called the *terminal state*.

Define a recursive function with prototype

~~~scala
/**
  * Find a sequence of legal moves of the frogs and toads puzzle from a specified starting
  * [[PuzzleState]] to the terminal [[PuzzleState]].
  *
  * @param start the starting [[PuzzleState]]
  * @return the sequence of [[PuzzleState]] objects passed through in the transit from
  * state `start` to the terminal state (inclusive). Returns the empty sequence if no solution
  * is found.
  */
def solve(start: PuzzleState): Seq[PuzzleState] = ...
~~~

which implements the following algorithm:

* If the state `start` is actually the terminal state of the game then declare success and return.
* Otherwise, **for each** move that is possible from state `start`, do the following:
  * make that move to give a new state which we call `next`,
  * make a recursive call `solve(next)` to try to find a solution to the problem starting from position `next`,
  * if that call was successful then declare success and return,
* If none of the possible moves tried resulted in success then declare failure and return.

Strictly speaking, the algorithm just described simply reports whether it has successfully found a solution. As it stands we haven't described what should be done to return the sequence of `PuzzleState` objects describing the sequence of states passed through from `start` state to the terminal state. If you decide to implement this algorithm as your solution then you will need to work out exactly what sequences of `PuzzleState` objects to return at each possible return point.

**Another way** to view this algorithm is to think of it as the following *generate and search* algorithm:

* First *generate* a tree in which
  * each node contains a `PuzzleState` object
  * the root of the tree contains the `PuzzleState` object that represents the initial state of the puzzle, with all frogs on the left and all toads on the right.
  * the `PuzzleState` objects in the children of a given node represent the states that can be reached from the `PuzzleState` in that node by making a single legal move (slide or jump).
* Now do a *depth first* search of the tree to find a node containing the `PuzzleState` object that represents the terminal state of the puzzle, with all toads on the left and all frogs on the right.

The sequence of puzzle states returned by the `solve` function would then be the sequence of `PuzzleState` objects on the nodes of the path from the root of this tree to the node we found that contained the terminal state object.

### A possible objection ###

One objection to the simple recursive function outlined in the last section is that it might be quite inefficient. It seems like it will waste a lot of time hunting off down pathways that it ultimately discovers to be dead ends.

This is certainly an issue with applying depth first searching in many problems, especially in areas like path planning and artificial intelligence. In this case, however, it turns out that if you try an incorrect move then then after only a few moves (maybe one or two) you will very quickly get to a point where you can go no further. So when this algorithm gets stuck it generally only needs to backtrack by a couple of moves before trying another move. In this case depth first searching is really quite efficient.

### A more strategic solution (advanced) ###

**Warning:** This section is more advanced, and you don't need anything here to do this assignment. You should only think about things in this section if you've already solved the assignment and are looking for some extra challenges.

We strongly encourage most people to implement a solution based upon the algorithm outlined above in their submission for assignment 1. But if you are really confident, or you would just like to think more about how you might **prove** that this puzzle always has a solution, then you might want to ponder the following points.

Take a look at the animated images above and observe that they reveal a clear strategy for jumping frogs and toads past each other. Notice in particular that:

* As the solution unfolds the empty cell shuttles backwards and forwards, moving as far as possible in one direction before it doubles back in the other direction.
* Most of the moves are jumps, which propagate the empty cell from one side to the other.
* The only place that a slide occurs is once at the end of each shuttle, as a preparation for the shuttle back in the other direction.

Does this suggest a different strategy for constructing a solution to the frogs and toads problem to you? Can you implement that strategy in Scala?

One advantage of the algorithm we discussed above is that it is relatively easy to see that if there *is* a solution to the puzzle then that algorithm will find it. Why is that the case?

Can you show that an algorithm based on the shuttling observations above will also construct a solution if there is one? Can you use it to show that a solution always exists?

### Source modules ###

This project contains three Scala modules:

1. [FrogsAndToads.scala](src/main/scala/FrogsAndToads.scala) this is a skeleton module in which you should put your code to solve the frogs and toads game. It currently just contains stub code for an `AnimationState` class and a `solve()` function.

2. [FrogsAndToadsTests.scala](src/test/scala/FrogsAndToadsTests.scala) this is a skeleton module in which you should put your automated tests of the functions you've provided in the [FrogsAndToads.scala](src/main/scala/FrogsAndToads.scala) module.

3. [Main.scala](src/main/scala/Main.scala) the driver module containing the `main()` entry point of the application. Also provides a utility function to take a sequence of animation frames, given as an object of type `Seq[doodle.image.Image]`, and to animate them in a window. This function has the following prototype:

~~~scala
/**
 * Animate a list of images in a shrink-to-fit window.
 *
 * @param title the text to display in the title bar of the window.
 * @param frames the list of `Image` objects to be displayed as the
 * sequence of frames in the animation.
 */
def runAnimation(title: String, frames: Seq[Image])
~~~

To compile and execute this code run the Scala [simple build tool (SBT)](https://www.scala-sbt.org/) from the root directory of this project and execute the `run` action to compile the project and run the `main()` method in the [Main.scala](src/main/scala/Main.scala) module.

---
[Dominic Verity](http://orcid.org/0000-0002-4137-6982)  
[Copyright (c) 2019 by Dominic Verity. Macquarie University. All rights reserved.](http://mozilla.org/MPL/2.0/)  
Last modified: 11 August 2019
