package com.battleship
import scala.io.StdIn._
/**
  * Created by danielimberman on 01/02/17.
  */

object BattleshipHelper{
  implicit class UserWithEither(u:User){
    def attack(t:Either[String, Point], defender:User) = {
      t match {
        case Left(l) =>
          println(s"error: $l")
          (u, defender, Response.error)
        case Right(r) =>
          u.attack(r, defender)
      }
    }
  }
}


object Battleship {
  import BattleshipHelper._

  def main(args: Array[String]) {
    println("Welcome to Battleship! Let's get started!")
    println("player 1 name? :")
    val player1 = readLine()
    println("player 2 name? :")
    val player2 = readLine()

    run(player1, player2)
  }

  def getAttack() = {
    println("attack with coordinates: x y")
    val line = readLine()
    try{
      val coordinates = line.split(" ").map(_.toInt)
      Right(Point(coordinates.head, coordinates(1)))
    }
    catch{ case e => Left(e.getLocalizedMessage)}
  }



  def addSpace = for(i<- 0 to 5) println()

  def run(p1name:String, p2name:String) = {
    def performTurn(attacker:User, defender:User):Unit = {
      println(s"${attacker.name}'s turn!")
      attacker.printBoard(attacking = true)
      val currentAttack = getAttack()
      val (currentAttacker, currentDefender, status) = attacker.attack(currentAttack, defender)
      println(status)
      println(currentAttacker)
      currentAttacker.printBoard(attacking = true)
      addSpace
      if(currentDefender.activePoints.isEmpty()) println(s"${attacker.name} wins!")
      else if(status == Response.error)  performTurn(currentAttacker,currentDefender)
      else performTurn(currentDefender,currentAttacker)

    }

    val u1 = User(p1name).placeShips()
    addSpace
    val u2 = User(p2name).placeShips()
    addSpace
    println("LETS PLAY BATTLESHIP!")
    performTurn(u1, u2)
  }

}
