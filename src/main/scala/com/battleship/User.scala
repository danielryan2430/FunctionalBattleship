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
  def isEmpty() = size == 0

}

case class User(name:String, activePoints:Grid = Grid.empty, attackPoints:Grid = Grid.empty, missedPoints:Grid = Grid.empty){
  def attack(p:Point, defender:User):(User, User,String) = {
    def successfulAttack= (User(name, activePoints, attackPoints.add(p), missedPoints), User(defender.name, defender.activePoints.remove(p), defender.attackPoints, defender.missedPoints),Response.hit)
    def missedAttack =  (User(name, activePoints, attackPoints, missedPoints.add(p)), defender,Response.miss)


    p match{
      case a if !BattleShipBoard.withinBounds(a) => (this, defender, Response.error)
      case b if defender.activePoints.contains(b) => successfulAttack
      case _ => missedAttack
    }
  }


  def printBoard(attacking:Boolean = false) = {
    if(attacking) println(BattleShipBoard.boardToGrid(attackPoints, missedPoints, Grid.empty))
    else println(BattleShipBoard.boardToGrid(Grid.empty, Grid.empty, activePoints))
  }

  def addShip(x:Int, y:Int, size:Int, vertical:Boolean) = {
    def createPointsFromInput: List[Point] = {
      if (vertical) List.range(y, y + size).map(p => Point(x, p))
      else List.range(x, x + size).map(p => Point(p, y))
    }
    def pointsAreInvalid(points:Seq[Point]): Boolean = {
      points.exists(p => activePoints.contains(p) || !BattleShipBoard.withinBounds(p)) || size < 1
    }

    val points = createPointsFromInput
    if(pointsAreInvalid(points)) Left((this, Response.boatPlaceErr))
    else Right((User(name, activePoints.addAll(points), attackPoints, missedPoints), Response.boatPlaced))
  }

  def parseShipFromUserInput() = {
    println("place ship: size x y vertical[true/false]")
    val line = readLine()
    try{
      val t = line.split(" ")
      Right(Ship(t(0).toInt,t(1).toInt,t(2).toInt, t(3).toBoolean))
    }
    catch{ case e:Exception => Left("error placing ship: " + e.getLocalizedMessage)}
  }


  def placeShip(u:User, numShips:Int):User = {
    def printErrorAndRetry(a: String): User = {
      println(a)
      placeShip(u, numShips)
    }
    def addShipAndContinue(s: Int, x: Int, y: Int, d: Boolean): User = {
      u.addShip(x, y, s, d) match{
        case Left((user, resp)) =>
          println(resp)
          user.printBoard(attacking = false)
          placeShip(user, numShips)
        case Right((user, resp)) =>
          println(resp)
          user.printBoard(attacking = false)
          placeShip(user, numShips - 1)

      }
    }

    def getShipFromUser: User = {
      parseShipFromUserInput() match {
        case Right(Ship(s, x, y, d)) => addShipAndContinue(s, x, y, d)
        case Left(a) => printErrorAndRetry(a)
      }
    }

    if(numShips == 0) u
    else getShipFromUser
  }

  def placeShips() = {
    println(s"placing ships for ${this.name}")
    placeShip(this,3)
  }
}
