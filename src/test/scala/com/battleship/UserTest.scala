package com.battleship

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by danielimberman on 01/02/17.
  */
class UserTest extends FlatSpec with Matchers{
  import UserGenerator._
  val userA = generate("a", activePoints = Set((1,1), (1,2), (1,3)) )
  val userB = generate("a",activePoints = Set((1,1), (2,1), (3,1)) )

  "attack" should "remove an active point from defending user if it exists" in {
    val (currentUser1, currentUser2, status) = userA.attack(Point(2,1),userB)
    currentUser2.activePoints shouldEqual Grid(Map(1 -> Set(1, 3)),2)
    currentUser1 shouldEqual generate(
      userA.name,
      activePoints =  Set((1,1), (1,2), (1,3)),
      attackPoints = Set((2,1)),Set()
    )
  }

  it should "only count as a misfire in cases where it is not in the defender" in {
    val (currentUser1, currentUser2, status) = userA.attack(Point(4,4),userB)
    currentUser2.activePoints shouldEqual Grid(Map(1 -> Set(1, 2, 3)),3)
    currentUser1 shouldEqual generate(
      userA.name,
      activePoints =  Set((1,1), (1,2), (1,3)),
      attackPoints = Set(),
      failedAttackPoints = Set((4,4))
    )
  }

  it should "contain numbers within the grid parameters" in {
    val (currentUser1, currentUser2, status) = userA.attack(Point(8,8),userB)
    currentUser1 shouldEqual userA
    currentUser2 shouldEqual userB
  }

}
