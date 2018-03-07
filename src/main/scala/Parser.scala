package a_4

import scala.util.parsing.combinator.RegexParsers

sealed trait ExprType
case object Nat extends ExprType
case object Bool extends ExprType
case class Prod(a: ExprType, b: ExprType) extends ExprType
case class Sum(a: ExprType, b: ExprType) extends ExprType
case class Fun(a: ExprType, b: ExprType) extends ExprType

sealed trait Expr
case class Var(name: String) extends Expr
case class Nat(value: Int) extends Expr
case class Bool(value: Boolean) extends Expr
case class App(left: Expr, righ: Expr) extends Expr
case class Lam(param: String, t: ExprType, body: Expr) extends Expr
case class Pair(left: Expr, right: Expr) extends Expr
case class Fst(e: Expr) extends Expr
case class Snd(e: Expr) extends Expr
case class Inl(e: Expr, t: ExprType) extends Expr
case class Inr(e: Expr, t: ExprType) extends Expr
case class Case(e: Expr, inlParam: String, inlExpr: Expr, inrParam: String, inrExpr: Expr) extends Expr

object Parser extends RegexParsers {
  val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  val v     = ident                              ^^ (x => Var(x))
  val nat   = """[0-9]+""".r                     ^^ (x => Nat(x.toInt))
  val bool  = """true|false""".r                 ^^ (x => Bool(x.toBoolean))
  val fst   = "fst" ~> "(" ~> expr <~ ")"        ^^ (Fst(_))
  val snd   = "snd" ~> "(" ~> expr <~ ")"        ^^ (Snd(_))

  // Parses either p1, p2 or p2, p1, but the result type is always Parser[(*p1,  *p2)]
  def anyOrder[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = (p1 ~ "," ~ p2 ^^ { case x ~ _ ~ y => (x, y) }) | (p2 ~ p1 ^^ { case x ~ y => (y, x) })

  val lam = ("\\" ~> ident <~ ":") ~ (exprType <~ ".") ~ expr                       ^^ { case v ~ t ~ e => Lam(v, t, e) }
  val inl   = ("inl" ~>! "(" ~> expr <~ ")") ~ (":" ~> exprType)                    ^^ { case e ~ t => Inl(e, t) }
  val inr   = ("inr" ~>! "(" ~> expr <~ ")") ~ (":" ~> exprType)                    ^^ { case e ~ t => Inr(e, t) }

  val caseInl = ("inl" ~>! ident <~ "=>") ~ expr                                    ^^ { case v ~ e => (v, e) }
  val caseInr = ("inr" ~>! ident <~ "=>") ~ expr                                    ^^ { case v ~ e => (v, e) }
  val casep = ("case" ~>! exprPrim <~! "of") ~ anyOrder(caseInl, caseInr)               ^^ { case e ~ (((inlp, inle), (inrp, inre))) => Case(e, inlp, inle, inrp, inre) }
  var pair = "(" ~> expr ~ "," ~ expr <~ ")"                                        ^^ { case e1 ~ _ ~ e2 => Pair(e1, e2) }

  lazy val exprPrim: Parser[Expr] = nat | bool | fst  | snd |
                                    lam | inl  | inr | casep | v | pair | "(" ~> expr <~ ")"

  lazy val expr: Parser[Expr] = rep1(exprPrim) ^^ (xs => xs.reduceLeft((x, y) => App(x, y)))

  // Type language parsers:
  val simpleType: Parser[ExprType] = "nat" ^^^ Nat | "bool" ^^^ Bool | "(" ~> exprType <~ ")"
  val prodType = rep1sep(simpleType, "*")   ^^ { case xs => xs.reduceLeft((x, y) => Prod(x, y)) }
  val sumType  = rep1sep(prodType, "+")     ^^ { case xs => xs.reduceLeft((x, y) => Sum(x, y)) }
  val exprType = rep1sep(sumType, "->")     ^^ { case xs => xs.reduceRight((x, y) => Fun(x, y)) }
}
