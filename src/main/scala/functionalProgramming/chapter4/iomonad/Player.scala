package functionalProgramming.chapter4.iomonad

import functionalProgramming.chapter4.iomonad.Console.ConsoleIO

case class Player(name : String, score : Int)

object Player {
/*
  Cette fonction calcule le gagnant et l'affiche :

  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner !")
    else if (p2.score > p1.score)
      println(s"${p2.name} is the winner !")
    else
      println("it's a draw.")

   On peut séparer le calcul de l'affichage pour avoir une fonction pure.
  */

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None

/*
  Cette fonction peut encore être factorisée puisqu'elle calcule le message à envoyer et l'envoie.

  def contest(p1: Player, p2: Player): Unit = winner(p1,p2) match {
    case Some(Player(name,_)) => println(s"$name is the winner !")
    case None => println("it's a draw.")
  }*/

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name,_) => s"$name is the winner !"
  } getOrElse "it's a draw."

  /*

  def contest(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1,p2)))
  */


/*  def PrintLine(msg: String): IO[Unit] =
    new IO[Unit] {
      override def run: Unit = println(msg)
    }*/

  def contest(p1: Player, p2: Player): Unit = PrintLine(winnerMsg(winner(p1,p2)))

  //************************************************************************************//

  def fahrenheitToCelsius(f : Double): Double =
    (f - 32) * 5.0/9.0

  def ReadLine: ConsoleIO[Option[String]] = Console.readLn

  def PrintLine(msg: String): ConsoleIO[Unit] = Console.printLn(msg)

  def convertText(text: Option[String]): ConsoleIO[Option[Double]] = Console.convertText(text)

  def converter: ConsoleIO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Farenheit: ")
    os <- ReadLine
    od <- convertText(os)
    _ <- od match {
      case Some(d) => PrintLine(fahrenheitToCelsius(d).toString)
      case None => PrintLine("This not a value in Farenheit !")
    }
  } yield ()
}
