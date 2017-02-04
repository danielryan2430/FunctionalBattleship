package com.battleship

/**
  * Created by danielimberman on 01/02/17.
  */
case class Point(x:Int, y:Int)




object Response extends Enumeration{
  val already_hit = "target already hit"
  val hit = "hit!"
  val error = "error"
  val miss = "miss"
  val boatPlaceErr = "cannot place boat there!"
  val boatPlaced = "placed boat!!"


}

object BattleShipBoard {
  val length = 5
  val width = 10


  def withinBounds(p:Point) = {
    val Point(x,y) = p
    x >= 0 && x < width && y >=0 && y < length
  }


  def addBorder(s:String) = {
    val rows = s.split("\n")

    val topBorder = List.range(0,width).foldLeft("B ")((s,i) => s + s"$i ")
    val rowsWithBorder = List.range(0,length).zip(rows).map{case( i,s) => s"$i " + s}.mkString("\n")
    topBorder + "\n" + rowsWithBorder

  }

  def convertStateToString(rowHits:Set[Int], rowMisses:Set[Int], rowActive:Set[Int]): String = {
    List.range(0, width).map(
      t =>
        t match {
          case a if rowHits.contains(a) => "X"
          case b if rowMisses.contains(b) => "O"
          case c if rowActive.contains(c) => "^"
          case _ => "*"
        }).mkString(" ")
  }

  def boardToGrid(hits:Grid, misses:Grid, active:Grid) = {
    //TODO find a better name for this function

    def generateRowString(ind: Int): String = {
      val rowHits = hits.getRow(ind)
      val rowMisses = misses.getRow(ind)
      val rowActive = active.getRow(ind)
      convertStateToString(rowHits, rowMisses, rowActive)
    }

    val rawValues = List.range(0,length).map(generateRowString).mkString("\n")
    addBorder(rawValues)
  }



}
