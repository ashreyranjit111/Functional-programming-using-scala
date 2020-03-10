/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Frogs and Toads puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.frogsandtoads

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FrogsAndToadsTests extends FlatSpec with Matchers {

  import PuzzleState._

  // Tests of an empty B-tree

  "An puzzle state with 5 frogs and 8 toads:" should
    "have 5 + 8 + 1 = 14 cells" in {
    assert(PuzzleState(5, 8).size == 14)
  }

  it should "have its empty cell at position 5" in {
    assertResult(5) {
      PuzzleState(5, 8).emptyLoc
    }
  }

  it should "be constructed in the initial puzzle state" in {
    assert(PuzzleState(5, 8).isInitialState())
  }

  it should "not be constructed in the terminal puzzle state" in {
    assert(!PuzzleState(5, 8).isTerminalState())
  }

  // FIXME Add more tests here.

  "move methods" should
    "slide right" in {
    assertResult(List(Frog, Empty, Frog, Toad, Toad)) { PuzzleState(2, 2).frogSlideRight().get.getBoard() }
  }

  it should
    "slide left" in {
    assertResult(List(Frog, Frog, Toad, Empty, Toad)) { PuzzleState(2, 2).toadSlideLeft().get.getBoard()}
  }

  it should
    "jump right" in {
    assertResult(List(Empty, Toad, Frog)) { PuzzleState(Vector(Frog, Toad, Empty)).frogJumpingRight().get.getBoard()}
  }

  it should
    "jump left" in {
    assertResult(List(Toad, Frog, Empty)) { PuzzleState(Vector(Empty, Frog, Toad)).toadJumpingLeft().get.getBoard()}
  }
}
