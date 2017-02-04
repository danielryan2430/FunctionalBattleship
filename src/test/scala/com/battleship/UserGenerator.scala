package com.battleship

/**
  * Created by danielimberman on 01/02/17.
  */
object UserGenerator {
  def pointFromTuple: ((Int, Int)) => Point = t => Point(t._1, t._2)

  def generate(name: String,
      activePoints: Set[(Int, Int)] = Set(),
      attackPoints: Set[(Int, Int)] = Set(),
      failedAttackPoints: Set[(Int, Int)] = Set()) = {
    User(name,
      activePoints.foldLeft(Grid.empty){case(grid, (x,y)) => grid.add(x,y)},
      attackPoints.foldLeft(Grid.empty){case(grid, (x,y)) => grid.add(x,y)},
      failedAttackPoints.foldLeft(Grid.empty){case(grid, (x,y)) => grid.add(x,y)}
    )

  }
}
