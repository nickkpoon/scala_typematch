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

    //case Var(a) => typeOf(a, Map(a, ExprType))
    case Var(name) => env(name)
    case Nat(_)  => Nat
    case Bool(_) => Bool

    /*case App(a, b)
      if (typeOf(a, env) == Nat) && typeOf(b, env) == Nat => Sum(typeOf(a, env),typeOf(a, env))

    case App(a, b)
      if (typeOf(a, env) == Nat) && typeOf(b, env) == Bool =>*/

    case Inl(a, b) => Sum(typeOf(a, env),b)
    /*case Inl(a, b)
      if typeOf(a, env) == Bool => Sum(Bool, b)*/
    case Inr(a, b) => Sum(b, typeOf(a, env))

    case App(a, b) => typeOf(a, env) match
    {
      case Fun(c, d) =>
      {
          if (typeOf(b, env) == c)
            Fun(typeOf(a, env), typeOf(b, env))
          else
          throw TypeException("App Error 1!")

      }
      case _ => throw TypeException("App Error 2!")

    }

     /* if (typeOf(a, env) != typeOf(b, env))
        throw TypeException("App Error!")
      else
        Fun(typeOf(a, env), typeOf(b, env))*/
    /*case Inr(a, b)
      if typeOf(a, env) == Bool => Sum(Nat, Bool)*/

    /*case Fst(a) => Prod(typeOf(a, env), _)
      if typeOf(a, env) == Prod(typeOf(a, env), _) => typeOf(a, env)*/

    case Pair(a,b) => Prod(typeOf(a, env), typeOf(b, env))

    case Fst(a) => typeOf(a, env)
    match
    {
      case Prod(c, d) => c
      case _ => throw TypeException("Fst Error!")
    }
    /*case Fst(a)
      if typeOf(a, env) == Prod(Bool, _) => Bool*/

    case Snd(a) => typeOf(a, env)
      match
      {
      case Prod(c, d) => d
      case _ => throw TypeException("Snd Error!")
    }
    /*case Snd(a)
      if typeOf(a, env) == Prod(Bool, Nat) => Nat*/
    /*case Lam(a, b, c)
      if typeOf(c, env) == Fun(_, typeOf(c, env)) => typeOf(c, env)*/

    //case Pair(a, b) => Prod(typeOf(a, env), typeOf(b, env))

    case Lam(a,b,c) => Fun(b, typeOf(c, env + (a->b)))
    //case Var(string) =>



    }


  }



