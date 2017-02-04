package com.battleship

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by danielimberman on 01/02/17.
  */


object BoardTester{
  implicit class AnswerTester(answer:String) extends FlatSpec with Matchers{
    def shouldFitBoardDimensions() = {
      answer.split("\n").length shouldEqual 5
      val rows = answer.split("\n")
      rows.map(_.replace(" ", "")).foreach(
        row =>{
          row.length shouldEqual  BattleShipBoard.width
        }
      )
    }

    def removeBorders() = {
      val rows = answer.split("\n")
      rows.tail.map(_.drop(2)).mkString("\n")
    }
  }
}


class BattleShipBoardTest extends FlatSpec with Matchers{

  import BoardTester._


  "printBoard" should "default to all asterisks" in {

    val answer = BattleShipBoard.boardToGrid(Grid.empty,Grid.empty, Grid.empty).removeBorders()
    answer.shouldFitBoardDimensions()
    val rows = answer.split("\n")

    rows.map(_.replace(" ","")).foreach(
      row => row.split("").foreach(_ shouldEqual "*")
    )
  }

  it should "mark all hits as X's" in {
    val hits = List.range(0,BattleShipBoard.length).foldLeft(Grid.empty)((g,y) => g.add(1,y))
    val answer = BattleShipBoard.boardToGrid(hits, Grid.empty, Grid.empty).removeBorders()

    answer.shouldFitBoardDimensions()
    println(answer)

    answer.split("\n").map(_.replace(" ","")).foreach(
      row =>{
        row(1) shouldEqual 'X'

        (row.head +: row.drop(2)).forall(_ == '*') shouldEqual true
      }
    )


  }



}
