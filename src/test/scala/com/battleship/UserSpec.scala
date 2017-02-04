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


  PropertyCheckConfig(minSize = 0, maxSize = 20)
  "addShip" should "always be within bounds" in {
    check(Prop.forAll(xRange, yRange, sizeRange, true) {
      case (x, initialY, size, vertical) => {
        val initialUser = User("a")
        val (newUser, response) = initialUser.addShip(x, initialY, size, vertical)

        def errorGiven: Boolean = {
          response == Response.boatPlaceErr && newUser == initialUser
        }

        def dimensionsInvalid: Boolean = {
          x < 0 ||
              initialY < 0 ||
              initialY + size > BattleShipBoard.length ||
              x > BattleShipBoard.width || size < 1
        }
        if (dimensionsInvalid)
          errorGiven
        else
          response == Response.boatPlaced &&
              List.range(initialY, initialY + size)
                  .forall(
                    y => newUser.activePoints.contains(Point(x,y))
                  )
      }
    }
    )
  }
}
