package a_4


object Main {

  // These types are pre-defined in the environment, in order to provide you
  // with something to test your typechecker with.  Do not change these.
  val defaultEnvironment = Map(
    "add"   -> Fun(Nat, Fun(Nat, Nat)),
    "even"  -> Fun(Nat, Bool),
    "const" -> Fun(Nat, Fun(Nat, Nat)),
    "twice" -> Fun(Fun(Nat, Nat), Fun(Nat, Nat)),
    "n2b"   -> Fun(Nat, Bool),
    "b2n"   -> Fun(Bool, Nat)
  )

  def main(args: Array[String]) {
    Parser.parse(Parser.expr, scala.io.StdIn.readLine) match {
      case Parser.Success(e,_)   => {
        // Feel free to println(e) if you need to see what the expression looks like,
        // but don't forget to remove it later!
        try {
          val t = TypeChecker.typeOf(e, defaultEnvironment)
          println(t)
        } catch {
          case TypeException(msg) => println(msg)
        }
      }
      case Parser.Failure(msg,_) => println("Syntax error: " + msg)
      case Parser.Error(msg,_)   => println("Syntax error: " + msg)
    }
  }
}