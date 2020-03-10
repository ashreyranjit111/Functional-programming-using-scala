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

/**
  * The top level object of the Frogs and Toads application.
  */


object Main {
  import cats.instances.all._
  import doodle.core._
  import doodle.image._
  import doodle.java2d._
  import monix.reactive._
  import doodle.java2d.effect._
  import doodle.interact.syntax._
  import Image._

  /**
    * Animate a list of images in a shrink-to-fit window.
    *
    * @param title the text to display in the title bar of the window.
    * @param frames the list of `Image` objects to be displayed as the
    * sequence of frames in the animation.
    */
  def runAnimation(title: String, frames: Seq[Image]) {
    val animation: Observable[Picture[Unit]] =
      Observable
        .fromIterable(
          frames.flatMap(Seq.fill(20)(_))
        )
        .map(Image.compile[Algebra,Drawing])

    val window: Frame =
      Frame.fitToPicture().background(Color.white).title(title)

    animation.animateFrames(window)
  }

  /**
    * Main entry point of the application.
    *
    * @param args the array of options and parameters passed on 
    * the command line.
    */
  def main(args: Array[String]) {
   runAnimation("Frogs and Toads", PuzzleState.animate(5, 8))

   

    /*val square =
        Image.square(100).fillColor(Color.green)

    val circle = 
        Image.circle(100).fillColor(Color.red)

    val square_animation = Seq(square beside circle)
    runAnimation("Square", square_animation )*/

    //runAnimation("Frogs and Toads", PuzzleState.animate(5, 8))
  }
}


