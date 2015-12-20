
abstract sealed class Action

case class Atom(atom: Unit => Action) extends Action {
  override def toString() = "atom"
}

case class Fork(a1: Action, a2: Action) extends Action {
  override def toString = s"fork ${a1 toString} ${a2 toString}"
}

case object Stop extends Action {
  override def toString = "stop"
}

class Concurrent[A](val func: (A => Action) => Action) {

  import Concurrent.roundRobin

  def andThen[B](after: Concurrent[B]): Concurrent[B] = flatMap(_ => after)

  def action(): Action = func(_ => Stop)

  def fork(): Concurrent[Unit] = ???
  def flatMap[B](mapper: A => Concurrent[B]): Concurrent[B] = ???

  def run(): () => Unit = roundRobin(List[Action](action))
}

object Concurrent {
  def apply[A](func: (A => Action) => Action) = new Concurrent[A](func)
  def of[A](a: A) = new Concurrent((cont: A => Action) => cont(a))
  
  def stop[A](): Concurrent[A] = ???
  def atom[A](ioA: Unit => A): Concurrent[A] = ???
  def par[A](c1: Concurrent[A], c2: Concurrent[A]): Concurrent[A] = ???

  private def roundRobin(list: List[Action]): () => Unit = ???
}
