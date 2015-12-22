
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

  // To convert 'func' to action we need stop work after function 'func' finishes work,
  // So we need pass to 'func' an empty continuation function (that will do nothing and returns Stop)
  def action(): Action = func(_ => Stop)

  def fork(): Concurrent[Unit] =
    Concurrent((cont: Unit => Action) => {
      // turn function 'func' into an an action
      val a1: Action = action()
      // get action from continuation function by applying ()
      val a2: Action = cont(())
      Fork(a1, a2)
    })

  def _flatMap[B](f: (A => Action) => Action, g: A => ((B => Action) => Action)):
      (B => Action) => Action = {

    // :: (B-> Action) -> A -> Action
    def lambda1(x : B => Action)(a : A): Action = g(a)(x)

    // :: (B-> Action) -> Action
    def lambda2(x : B => Action): Action = f(lambda1(x))

    lambda2
  }

  def flatMap[B](mapper: A => Concurrent[B]): Concurrent[B] = ???


  def run(): () => Unit = roundRobin(List[Action](action))
}

object Concurrent {
  def apply[A](func: (A => Action) => Action) = new Concurrent[A](func)
  def of[A](a: A) = new Concurrent((cont: A => Action) => cont(a))
  
  def stop[A](): Concurrent[A] = Concurrent(_ => Stop)
  def atom[A](ioA: Unit => A): Concurrent[A] = Concurrent((cont: A => Action) => cont(ioA()))

  def par[A](c1: Concurrent[A], c2: Concurrent[A]): Concurrent[A] = ???
//    Concurrent((cont: A => Action) => {
//      val a1: Fork = c1.fork().action()
//      val a2: Action = c2.fork().action()
//      Fork(a1,a2)
//  })

  private def roundRobin(list: List[Action]): () => Unit = ???
}
