/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.frogsandtoads

import doodle.core._
import doodle.syntax._
import doodle.image._

/**
  * A puzzle state is given as a 1-dimensional array of cell values.
  */
class PuzzleState private ( private val board: Vector[PuzzleState.Cell], val emptyLoc: Int) {

  import PuzzleState._

  val size = board.size


  def getBoard(): List[PuzzleState.Cell] = {
    board.toList
  }

  def isTerminalState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Toad) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Frog)
  }

  def isInitialState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Frog) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Toad)
  }

  override def toString() =
    "[" ++ board.map(_.toString).reduceLeft(_ ++ "|" ++ _) + "]"

  // FIXME you might want to add more methods here.

  //for toad to slide one left if _T
  /*
  The toadSlideLeft() function call will only happen in the following cases:
  - the Toad is the exactlly beside right side of the empty space
  - the index of the empty space is within the bound of the size of the board
  */

  def toadSlideLeft() : Option[PuzzleState] ={
    if(emptyLoc + 1 >= 0 && emptyLoc + 1 < size && board.apply(emptyLoc + 1)==Toad)
    {
      val newPuzzleState = board.slice(0, emptyLoc) ++ Vector(Toad, Empty)++board.slice(emptyLoc+2,size)
      val newState = Some(new PuzzleState(newPuzzleState, emptyLoc+1))
      newState
    }
    else
    {
      None
    }
  }

  //for frog to slide one right if F_
  /*
  The frogSlideRight() function call will only happen in the following cases:
  - the Frog is exactlly beside left side of the empty space
  - the index of the empty space is within the bound of the size of the board
  */


  def frogSlideRight() : Option[PuzzleState] ={
    //if(getBoard(emptyLoc - 1)==Frog)
    //return Some(new PuzzleState(Vector(), 0))
    if(emptyLoc - 1 >= 0 && emptyLoc - 1 < size && board.apply(emptyLoc - 1) == Frog)
    {
      val newPuzzleState = board.slice(0, emptyLoc - 1) ++ Vector(Empty, Frog) ++ board.slice(emptyLoc + 1,size)
      Some(new PuzzleState(newPuzzleState, emptyLoc-1))
    }
    else
    {
      None
    }
  }

  //for toad to jump two left if _FT
  /*
  The toadJumpingLeft() function call will only happen in the following cases:
  - the Toad is the exactlly two space right side of the empty space &&
  - the Frog is exactlly one space beside right side of the empty space( i.e. the Frog is in between the Toad and the empty space )
  - the index of the empty space is within the bound of the size of the board
  */

  def toadJumpingLeft() : Option[PuzzleState] ={
    if(emptyLoc + 2 >= 0 && emptyLoc + 2 < size && board.apply(emptyLoc+2)==Toad && board.apply(emptyLoc+1)==Frog){

        val newPuzzleState = board.slice(0, emptyLoc) ++ Vector(Toad, Frog, Empty) ++ board.slice(emptyLoc+3, size)
        val newState = Some(new PuzzleState(newPuzzleState, emptyLoc+2))
        newState

    }else{
      None
    }
  }

  //for frog to jump two right if FT_
  /*
  The frogJumpingRight() function call will only happen in the following cases:
  - the Frog is the exactlly two space left side of the empty space &&
  - the Toad is exactlly one space beside left side of the empty space( i.e. the Toad is in between the Frog and the empty space )
  - the index of the empty space is within the bound of the size of the board
   */

  def frogJumpingRight() : Option[PuzzleState] = {
    if(emptyLoc - 2 >= 0 && emptyLoc -2 < size && board.apply(emptyLoc-2)==Frog && board.apply(emptyLoc-1)==Toad)
    {

        val newPuzzleState = board.slice(0, emptyLoc-2)++ Vector(Empty, Toad, Frog)++ board.slice(emptyLoc+1, size)
        Some(new PuzzleState(newPuzzleState, emptyLoc-2))

    }
    else{
      None
    }
  }
}

/**
  * Companion object for the [[PuzzleState]] class, provides a public constructor.
  */
object PuzzleState {


  /**
    * Base class for case objects to represent the possible contents of a
    * cell in the puzzle.
    */
  sealed abstract class Cell {
    override def toString(): String =
      this match {
        case Frog => "F"
        case Toad => "T"
        case Empty => " "
      }
  }

  case object Frog extends Cell
  case object Toad extends Cell
  case object Empty extends Cell


  /**
    * Construct a [[PuzzleState]] object in the initial state for a
    * puzzle with specified numbers of frogs and toads.
    *
    * @param frogs number of frogs to place on the left of the [[PuzzleState]]
    * to be constructed
    * @param toads number of toads top place on the right of the [[PuzzleState]]
    * to be constructed
    */
  def apply(frogs: Int, toads: Int): PuzzleState = {
    if (frogs <= 0 || frogs > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    if (toads <= 0 || toads > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    new PuzzleState(
      Vector.fill(frogs)(Frog) ++ Vector(Empty) ++
        Vector.fill(toads)(Toad),
      frogs
    )
  }

  def apply(board: Vector[Cell]): PuzzleState = new PuzzleState(board, board.indexOf(Empty))

  /**
    * Find a sequence of legal moves of the frogs and toads puzzle from a specified starting
    * [[PuzzleState]] to the terminal [[PuzzleState]].
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[PuzzleState]] objects passed through in the transit from
    * state `start` to the terminal state (inclusive). Returns the empty sequence if no solution
    * is found.
    */
  def solve(start: PuzzleState): Seq[PuzzleState] = {
    println(start.getBoard())

    println(start.frogJumpingRight().isDefined + " " + start.toadSlideLeft().isDefined + " " + start.toadJumpingLeft().isDefined + " " + start.frogSlideRight().isDefined)

    if (start.isTerminalState()) {
      return Seq(start)
    } else {
      if (start.frogSlideRight().isDefined) {
        return Seq(start) ++ solve(start.frogSlideRight().get)
      } else if (start.toadJumpingLeft().isDefined) {
        return Seq(start) ++ solve(start.toadJumpingLeft().get)
      }
      else if (start.toadSlideLeft().isDefined) {
        return Seq(start) ++ solve(start.toadSlideLeft().get)
      } else if (start.frogJumpingRight().isDefined) {
        return Seq(start) ++ solve(start.frogJumpingRight().get)
      }else {
        return Seq()
      }
    }
  }

  /**
    * Call [[solve]] to generate a sequence of legal moves from a specified
    * starting [[PuzzleState]] to the terminal [[PuzzleState]]. Render each state in that solution as
    * an image and return the resulting sequence of images.
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[Image]] objects depicting the sequence of puzzle states
    * passed through in the transit from the `start` state to the terminal state.
    */
  def animate(start: PuzzleState): Seq[Image] = {
    // FIXME add your code here to generate the animation frame sequence.
    val result = solve(start)
    result.foreach(i => println(i))
    result.map(i => drawBoard(i.getBoard()))
  }

//gets the list and returns the lists as Images
  def drawBoard(board: List[Cell]): Image = board match {
    case Nil => Image.empty
    case Frog :: tail => Image.square(20).fillColor(Color.blue).beside(drawBoard(tail)) // representing frog with blue box
    case Empty :: tail => Image.square(20).fillColor(Color.white).beside(drawBoard(tail)) // representing empty space with white box
    case Toad :: tail => Image.square(20).fillColor(Color.green).beside(drawBoard(tail)) //representing the toad with green color box
  }

  /**
    * Create an animation of a solution to the frogs and toads puzzle, starting from the initial
    * [[PuzzleState]] and ending at the terminal [[PuzzleState]].
    *
    * @param frogs the number of frogs in the puzzle (between 1 and 10 inclusive)
    * @param toads the number of toads in the puzzle (between 1 and 10 inclusive)
    */
  def animate(frogs: Int, toads: Int): Seq[Image] =
    animate(PuzzleState(frogs, toads))


  //FIXME You might want to add some (private) auxiliary functions here.
}
