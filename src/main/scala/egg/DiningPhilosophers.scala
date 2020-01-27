package egg

import zio._

object DiningPhilosophers extends App {

  import zio.console._
  import zio.stm._

  final case class Fork(number: Int)

  final case class Placement(left: TRef[Option[Fork]],
                             right: TRef[Option[Fork]])

  final case class RoundTable(seats: Vector[Placement])

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val count = 10

    def eaters(table: RoundTable): Iterable[ZIO[Console, Nothing, Unit]] =
      (0 to count).map { index =>
        eat(index, table)
      }

    for {
      table <- setupTables(count)
      fiber <- ZIO.forkAll(eaters(table))
      _ <- fiber.join
      _ <- putStrLn("All philosophers have eaten")
    } yield 0
  }

  def eat(philosopher: Int,
          roundTable: RoundTable): ZIO[Console, Nothing, Unit] = {
    val placement = roundTable.seats(philosopher)

    val left = placement.left
    val right = placement.right

    for {
      forks <- takeForks(left, right).commit
      _ <- putStrLn(s"Philosopher ${philosopher} eating...")
      _ <- putForks(left, right)(forks).commit
      _ <- putStrLn(s"Philosopher ${philosopher} is finished.")
    } yield ()
  }

  def setupTables(size: Int): ZIO[Any, Nothing, RoundTable] = {
    def makeFork(i: Int) = TRef.make[Option[Fork]](Some(Fork(i)))

    (for {
      allForks0 <- STM.foreach(0 to size) { i =>
        makeFork(i)
      }
      allForks = allForks0 ++ List(allForks0(0))
      placements = (allForks zip allForks.drop(1)).map {
        case (l, r) => Placement(l, r)
      }
    } yield RoundTable(placements.toVector)).commit
  }

  def takeForks(left: TRef[Option[Fork]],
                right: TRef[Option[Fork]]): STM[Nothing, (Fork, Fork)] =
    left.get.collect { case Some(fork) => fork } zip right.get.collect {
      case Some(fork)                  => fork
    }

  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(
      tuple: (Fork, Fork)): STM[Nothing, Unit] = {
    val (leftFork, rightFork) = tuple
    for {
      _ <- right.set(Some(rightFork))
      _ <- left.set(Some(leftFork))
    } yield ()
  }

}
