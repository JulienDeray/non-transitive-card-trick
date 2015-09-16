import scala.util.Random

val cards = {
  val r = Random

  for {
    i <- List.range(0, 52)
    card = r.nextBoolean()
  } yield card
}

type PlayerChoice = (Boolean, Boolean, Boolean)

case class Player(name: String, choice: PlayerChoice, score: Int)

val player1 = Player("UN", (false, true, true), 0)
val player2 = Player("DEUX", (true, true, true), 0)

def runGame(p1: Player, p2: Player, cards: Seq[Boolean]): Player = {
  cards match {
    case p1.choice._1 :: p1.choice._2 :: p1.choice._3 :: tail => runGame(Player(p1.name, p1.choice, p1.score + 1), p2, tail)
    case p2.choice._1 :: p2.choice._2 :: p2.choice._3 :: tail => runGame(p1, Player(p2.name, p2.choice, p2.score + 1), tail)

    case card1 :: p1.choice._1 :: p1.choice._2 :: p1.choice._3 :: tail => runGame(Player(p1.name, p1.choice, p1.score + 1), p2, card1 :: tail)
    case card1 :: p2.choice._1 :: p2.choice._2 :: p2.choice._3 :: tail => runGame(p1, Player(p2.name, p2.choice, p2.score + 1), card1 :: tail)

    case card1 :: card2 :: p1.choice._1 :: p1.choice._2 :: p1.choice._3 :: tail => runGame(Player(p1.name, p1.choice, p1.score + 1), p2, card1 :: card2 :: tail)
    case card1 :: card2 :: p2.choice._1 :: p2.choice._2 :: p2.choice._3 :: tail => runGame(p1, Player(p2.name, p2.choice, p2.score + 1), card1 :: card2 :: tail)

    case head :: tail => runGame(p1, p2, tail)
    case _ => if (p1.score > p2.score) p1 else p2
  }
}

println( runGame(player1, player2, cards) )
