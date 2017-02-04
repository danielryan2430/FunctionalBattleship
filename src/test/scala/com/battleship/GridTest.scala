package com.battleship

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by danielimberman on 02/02/17.
  */
class GridTest extends FlatSpec with Matchers {
  "addAll" should "add an array of points" in {
    val points = List.range(0,3).map(y => Point(1,y))
    Grid.empty.addAll(points) shouldEqual Grid(Map(0 -> Set(1), 1 -> Set(1), 2 -> Set(1)),3)
  }

}
