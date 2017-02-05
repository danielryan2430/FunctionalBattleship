package com.battleship

import org.scalacheck.{Prop, Gen}
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.prop.Checkers

/**
  * Created by danielimberman on 04/02/17.
  */
class UserSpec extends FlatSpec with Checkers {
  val xRange = Gen.choose(0, 20)
  val yRange = Gen.choose(0, 20)
  val sizeRange = Gen.choose(0, 20)

  def userContainsShip(u:User, initialX:Int, initialY:Int, size:Int, vertical:Boolean) = {
    if(vertical)List.range(initialY, initialY + size).forall(y => u.activePoints.contains(Point(initialX,y)))
    else List.range(initialX, initialX + size).forall(x => u.activePoints.contains(Point(x,initialY)))
  }



  PropertyCheckConfig(minSize = 0, maxSize = 20)
  "addShip" should "always be within bounds" in {
    check((vertical:Boolean) => Prop.forAll(xRange, yRange, sizeRange) {
      case (x, y, size) => {
        val initialUser = User("a")
        val (newUser, response) = initialUser.addShip(x, y, size, vertical)

        def errorGiven: Boolean = {
          response == Response.boatPlaceErr && newUser == initialUser
        }
        println(newUser)

        def dimensionsInvalid: Boolean = {
          x < 0 ||
              y < 0 ||
              y + size > BattleShipBoard.length ||
              x > BattleShipBoard.width || size < 1
        }
        if (dimensionsInvalid)
          errorGiven
        else response == Response.boatPlaced && userContainsShip(newUser, x, y, size, vertical)

      }
      }
    )
  }
}
