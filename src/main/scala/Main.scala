import scala.util.Random
import Concurrent._

object Main extends App {

  val x01 = Concurrent((_: Unit => Action) => Stop).action

//  val x2 = Concurrent((_: Unit => Action) => Fork(Stop(), Fork(Stop(), Stop()))).action
//  val x3 = Concurrent((_ : Unit => Action) => Atom(putStr("Haskell")))
//  val x45 = Concurrent((_: Unit => Action) => Atom(putStr("Haskell") andThen (_ => Stop()))).action
//  val x6 = stop.action
//  val x7 = stop
//  val x8 = atom(putStrLn("Haskell")).action
//  val x9 = atom(_ => ???).action
//  val x10 = atom(putStr("Haskell"))
//  val x11 = stop.fork.action
//  val x1213 = atom(putStr("Haskell")).fork.action
//  val x14 = ???.fork.action
//  val x15 = par(stop(), stop()).action
//  val x16 = par(atom(putStr("think")), atom(putStr("hack"))).action
//  val x17 = par(stop(), stop.fork).action
//  val x18 = par(atom(putChar('x')), stop.fork).action
//  val x19 = stop.flatMap(c => stop[Unit]).action
//  val x20 = atom(putStrLn("whatever...")).flatMap(stop).action
//  val x2122 = stop.flatMap(stop)
//  val x23 = stop.fork.flatMap(_ => stop.fork).action
//  val x24 = ex0 run
//  val x25 = ex1 run

  def ex0(): Concurrent[Unit] = par(loop(genRandom(1337)), loop(genRandom(2600)) andThen atom(putStrLn("")))

  def ex1(): Concurrent[Unit] = atom(putStr("Haskell")) andThen loop(genRandom(7331)).fork andThen loop(genRandom(42)) andThen atom(putStrLn(""))

  def genRandom(s: Int): List[Int] = s match {
    case 42 => List(71, 71, 17, 14, 16, 91, 18, 71, 58, 75)
    case 1337 => List(1, 96, 36, 11, 42, 47, 9, 1, 62, 73)
    case 2600 => List(83, 98, 35, 84, 44, 61, 54, 35, 83, 9)
    case 7331 => List(17, 73, 92, 36, 22, 72, 19, 35, 6, 74)
    case _ => {
      val rand = new Random(s)
      (0 to 9).map(_ => rand.nextInt(99)).toList
    }
  }

  def loop(list: List[Int]): Concurrent[Unit] = list.map(String.valueOf(_))
    .map(putStr(_))
    .map(atom(_))
    .foldRight(Concurrent.of())(_ andThen _)

  def putStr(line: String): Unit => Unit = _ => print(line)

  def putStrLn(line: String): Unit => Unit = _ => println(line)

  def putChar(c: Char): Unit => Unit = _ => print(c)
}