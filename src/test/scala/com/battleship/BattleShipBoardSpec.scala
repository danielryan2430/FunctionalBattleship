package com.battleship

import org.scalacheck.Arbitrary._
import org.scalatest.junit.JUnitSuite

import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers


/**
  * Created by danielimberman on 04/02/17.
  */
class BattleShipBoardSpec extends FlatSpec with Checkers{

  def fitsRow = (i:Int) => i > 0 && i <= BattleShipBoard.width

  "rowToGrid" should "place in hits wherever specified" in {
    check((hits:Set[Int], misses:Set[Int], active:Set[Int]) =>{
      val row = BattleShipBoard.convertStateToString(hits,misses,active).split(" ")
      val validHits = hits.filter(fitsRow)
      val validMisses = misses.filter(fitsRow).diff(validHits)
      val validActive = active.filter(fitsRow).diff(validHits).diff(validMisses)

      val hitsAppear = validHits.forall(h => row(h) == "X")
      val missesAppear = validMisses.forall(h => row(h) == "0")
      val activeAppear = validActive.forall(h => row(h) == "^")
      hitsAppear && missesAppear && activeAppear
    })
  }


}
