package com.battleship

import scala.io.StdIn._

/**
  * Created by danielimberman on 01/02/17.
  */


object Grid{
  val empty = Grid(Map().withDefaultValue(Set()))
}

case class Ship(size:Int, x:Int, y:Int, vertical:Boolean)

case class Grid(values:Map[Int,Set[Int]], size:Int = 0){

  def getRow(i:Int) = values(i)
  def contains(p:Point) = values(p.y).contains(p.x)
  def add(x:Int,y:Int):Grid = Grid(values + (y -> (values(y) + x)), size + 1)
  def addAll(p:Seq[Point]) = p.foldLeft(this)((g,a) => g.add(a))
  def add(p:Point):Grid = add(p.x,p.y)
  def remove(x:Int,y:Int):Grid = Grid(values + (y -> (values(y) - x)), size - 1)
  def remove(p:Point):Grid = remove(p.x,p.y)

}

case class User(name:String, activePoints:Grid = Grid.empty, attackPoints:Grid = Grid.empty, missedPoints:Grid = Grid.empty){
  def attack(p:Point, defender:User):(User, User,String) = {
    def successFulAttack= (User(name, activePoints, attackPoints.add(p), missedPoints), User(defender.name, defender.activePoints.remove(p), defender.attackPoints),Response.hit)
    def missedAttack =  (User(name, activePoints, attackPoints, missedPoints.add(p)), defender,Response.miss)


    p match{
      case a if !BattleShipBoard.withinBounds(a) => (this, defender, Response.error)
      case b if defender.activePoints.contains(b) => successFulAttack
      case _ => missedAttack
    }
  }


  def printBoard(attacking:Boolean = false) = {
    if(attacking) println(BattleShipBoard.boardToGrid(attackPoints, missedPoints, Grid.empty))
    else println(BattleShipBoard.boardToGrid(Grid.empty, Grid.empty, activePoints))
  }

  def addShip(x:Int, y:Int, size:Int, vertical:Boolean) = {
    val points = if(vertical) List.range(y, y + size).map(p => Point(x,p))
    else  List.range(x, x + size).map(p => Point(p,y))

    val b= !BattleShipBoard.withinBounds(Point(11,0))
    val c = points.exists(p => !BattleShipBoard.withinBounds(p))

    if(points.exists(p => activePoints.contains(p) || !BattleShipBoard.withinBounds(p)) || size == 0){
      (this, Response.boatPlaceErr)
    }
    else{
      (User(name, activePoints.addAll(points), attackPoints, missedPoints), Response.boatPlaced)
    }



  }

  def getShip(size:Int) = {
    val line = readLine()
    try{
      val t = line.split(" ")
      Right(Ship(size,t(0).toInt,t(1).toInt, t(2).toBoolean))
    }
    catch{ case e => Left(e.getLocalizedMessage)}
  }


  def placeShip(u:User, size:Int):User = {
    getShip(size) match {
      case Right(Ship(s,x,y,d)) =>
        val (user,resp) = u.addShip(x,y,s,d)
        println(resp)
        user.printBoard(attacking = false)
        user
      case Left(a) =>
        println(a)
        placeShip(u,size)
    }
  }

  def placeShips() = {
    println(s"placing ships for ${this.name}")
    List.fill(3)(0).foldLeft(this)((u,_) => placeShip(u,4))
  }
}
