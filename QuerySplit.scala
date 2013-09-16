import scala.collection.immutable.{Map, HashMap}

sealed abstract class Formula

case class And(arg1: Formula, arg2: Formula) extends Formula {
  override def toString = "(and " + arg1 + " " + arg2 + ")"
}

case class Or(arg1: Formula, arg2: Formula) extends Formula {
  override def toString = "(or " + arg1 + " " + arg2 + ")"
}

case class Not(arg: Formula) extends Formula {
  override def toString = "(not " + arg + ")"
}

case class Variable(name: String) extends Formula {
  override def toString = name
}

sealed abstract class Literal extends Formula { def unary_!(): Literal }
case object T extends Literal { def unary_! = F }
case object F extends Literal { def unary_! = T }

object QuerySplit {

  def computeFilter(exp: Formula) = simplify(biasTo(T, simplify(exp)))

  def biasTo(bias: Literal, exp: Formula): Formula = {
    exp match {
      case T | F           => exp
      case Variable(name)  => if (name(0) == 'v') exp else bias
      case And(arg1, arg2) => And(biasTo(bias, arg1), biasTo(bias, arg2))
      case Or(arg1, arg2)  => Or(biasTo(bias, arg1), biasTo(bias, arg2))
      case Not(arg)        => Not(biasTo(!bias, arg))
    }
  }

  def simplify(exp: Formula): Formula = {
    exp match {
      case T | F | Variable(_) => exp
      case And(arg1, arg2)     => simplifyJunction(arg1, arg2, T)
      case Or(arg1, arg2)      => simplifyJunction(arg1, arg2, F)
      case Not(arg)            => simplifyNot(arg)
    }
  }

  def simplifyJunction(arg1: Formula, arg2: Formula, id: Literal) = {
    (simplify(arg1), simplify(arg2)) match {
      case (arg, _) if (arg == !id)            => !id
      case (_, arg) if (arg == !id)            => !id
      case (arg1, arg2) if (arg1 == id)        => arg2
      case (arg1, arg2) if (arg2 == id)        => arg1
      case (arg1, arg2) if (arg1 == arg2)      => arg1
      case (arg1, Not(arg2)) if (arg1 == arg2) => !id
      case (Not(arg1), arg2) if (arg1 == arg2) => !id
      case (Not(arg1), Not(arg2))              => Not(if (id == T) Or(arg1, arg2) else And(arg1, arg2))
      case (arg1, arg2)                        => if (id == T) And(arg1, arg2) else Or(arg1, arg2)
    }
  }

  def simplifyNot(arg: Formula) = {
    simplify(arg) match {
      case T      => F
      case F      => T
      case Not(x) => x
      case x      => Not(x)
    }
  }

  def rewriteOneArg(arg1: Formula, arg2: Formula, id: Literal): (Formula, Formula) = {
    val new1 = rewrite(arg1, mustBe(id, arg2))
    val new2 = rewrite(arg2, mustBe(id, arg1))
    if ((new1 != arg1) && (new2 != arg2)) {
      if (countLiterals(new1) > countLiterals(new2)) (new1, arg2) else (arg2, new2)
    } else {
      (new1, new2)
    }
  }

  def rewrite(exp: Formula, mustBe: Map[Formula, Literal]): Formula = {
    mustBe.getOrElse(exp, {
      exp match {
        case T | F | Variable(_) => exp
        case And(arg1, arg2)     => And(rewrite(arg1, mustBe), rewrite(arg2, mustBe))
        case Or(arg1, arg2)      => Or(rewrite(arg1, mustBe), rewrite(arg2, mustBe))
        case Not(arg)            => Not(rewrite(arg, mustBe))
      }
    })
  }

  def countLiterals(exp: Formula): Int = {
    exp match {
      case T | F           => 1
      case Variable(_)     => 0
      case And(arg1, arg2) => countLiterals(arg1) + countLiterals(arg2)
      case Or(arg1, arg2)  => countLiterals(arg1) + countLiterals(arg2)
      case Not(arg)        => countLiterals(arg)
    }
  }

  def mustBe(value: Literal, exp: Formula): Map[Formula, Literal] = {
    Map(exp -> value) ++
    (exp match {
      case T | F | Variable(_) => Map.empty
      case And(arg1, arg2) => if (value == T) mustBe(T, arg1) ++ mustBe(T, arg2) else Map.empty
      case Or(arg1, arg2)  => if (value == F) mustBe(F, arg1) ++ mustBe(F, arg2) else Map.empty
      case Not(arg)        => mustBe(!value, arg)
    })
  }

  def check(exp: Formula) = {
    println(exp + " => " + computeFilter(exp))
  }

  def main(args: Array[String]) {
    val v1 = Variable("v1")
    val v2 = Variable("v2")
    val w1 = Variable("w1")
    val w2 = Variable("w2")


    check(T)           // T
    check(F)           // F
    check(v1)          // v1
    check(w1)          // w1
    check(And(v1, v2)) // (and v1 v2)
    check(And(w1, w2)) // (and w1 w2)
    check(And(v1, w2)) // (and v1 w2)
    check(Or(v1, v2))  // (or v1 v2)
    check(Or(w1, w2))  // (or w1 w2)
    check(Or(v1, w2))  // (or v1 w2)
    check(Not(v1))     // (not v1)
    check(Not(w2))     // (not w2)
  }

}
