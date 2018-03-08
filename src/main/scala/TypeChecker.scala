package a_4

final case class TypeException(msg: String) extends Exception("Type error: " + msg, None.orNull)

object TypeChecker {

  // Throws a TypeException with a message
  def typeError(msg: String): Nothing = throw TypeException(msg)

  // returns the type of an expression under environment `env`.
  // This is your type checker, it should either:
  // -- return the type of the expression, if it is well typed
  // -- throw TypeException if the program is not well typed at any point.


  //def typeOf(prog: Expr, env: Map[String, ExprType]): ExprType = ???
  def typeOf(prog: Expr, env: Map[String, ExprType]): ExprType = prog match
    {


    case Nat(_)  => Nat
    case Bool(_) => Bool

    /*case App(a, b)
      if (typeOf(a, env) == Nat) && typeOf(b, env) == Nat => Sum(typeOf(a, env),typeOf(a, env))

    case App(a, b)
      if (typeOf(a, env) == Nat) && typeOf(b, env) == Bool =>*/

    case Inl(a, b)
      if typeOf(a, env) == Nat => Sum(Nat,Bool)
    case Inl(a, b)
      if typeOf(a, env) == Bool => Sum(Bool, Nat)
    case Inr(a, b)
      if typeOf(a, env) == Nat => Sum(Bool, Nat)
    case Inr(a, b)
      if typeOf(a, env) == Bool => Sum(Nat, Bool)

    case Fst(a)
      if typeOf(a, env) == Prod(Nat, Bool) => Nat
    case Fst(a)
      if typeOf(a, env) == Prod(Bool, Nat) => Bool

    case Snd(a)
      if typeOf(a, env) == Prod(Nat, Bool) => Bool
    case Snd(a)
      if typeOf(a, env) == Prod(Bool, Nat) => Nat



    }


  }



