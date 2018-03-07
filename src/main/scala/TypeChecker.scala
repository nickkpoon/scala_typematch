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
    case Nat(a)  => Nat
    case Bool(a) => Bool
    /*case Prod(a, b) => a = typeOf(a, b)
      b = typeOf(b, b)
    case Fun (a, b) => if (typeOf(a) == Nat) =>
    case Sum (a, b) =>
    case Prod (a, b) =>*/

    }


  }



