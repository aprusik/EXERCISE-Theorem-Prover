import CNFParser._

import CNFSyntax._

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Unify {
  def apply(c1: Clause, c2: Clause): Option[List[(Clause, Clause)]] = {
    var unified:  List[(Clause, Clause)] = Nil
    var i1 = 0
    var i2 = 0

    var newC1: Clause = c1.deepCopy()
    var newC2: Clause = c2.deepCopy()
    val oldBuffer: ArrayBuffer[Term] = Term.usedTerms.map(_.deepCopy())

    def unifyLiterals(l1: MyLiteral, l2: MyLiteral): Unit = {
//    for (l1 <- newC1.literals; l2 <- newC2.literals) {
      if (l1.name == l2.name && l1.negated != l2.negated) {
        //        @tailrec
        def unifyTerms(tl1: => ArrayBuffer[Term], tl2: => ArrayBuffer[Term]): Boolean = {
          if (tl1.isEmpty && tl2.isEmpty) {
            true
          } else {
            val t = (tl1.head, tl2.head)
            t match {
              case (Constant(con1), Constant(con2)) if con1 != con2 => false // fail
              case (Constant(con1), Constant(con2)) if con1 == con2 => // do nothing
                newC1.update()
                newC2.update()
                unifyTerms(tl1.tail, tl2.tail)
              case (Constant(_), Function(_, _)) => false // fail
              case (con: Constant, v: Variable) => // substitute con for v
                v.substitute(con)
                newC1.update()
                newC2.update()
                unifyTerms(tl1.tail, tl2.tail)
              case (v: Variable, con: Constant) => // substitute con for v
                v.substitute(con)
                newC1.update()
                newC2.update()
                unifyTerms(tl1.tail, tl2.tail)
              case (Function(f1, _), Function(f2, _)) if f1 != f2 => false // fail
              case (Function(f1, ft1), Function(f2, ft2)) if f1 == f2 => // unify argument lists
                unifyTerms(ft1, ft2)
                newC1.update()
                newC2.update()
                unifyTerms(tl1.tail, tl2.tail)
              case (f: Function, Variable(v)) if f.contains(v) => false // fail
              case (Variable(v), f: Function) if f.contains(v) => false // fail
              case (f: Function, v: Variable) => // substitute f for v
                v.substitute(f)
                newC1.update()
                newC2.update()
                unifyTerms(tl1.tail, tl2.tail)
              case (v: Variable, f: Function) => // substitute f for v
                v.substitute(f)
                newC1.update()
                newC2.update()
                unifyTerms(tl1.tail, tl2.tail)
              case (v1: Variable, v2: Variable) => // substitute v1 for v2
                v2.substitute(v1)
                newC1.update()
                newC2.update()
                unifyTerms(tl1.tail, tl2.tail)
              case _ => false // everything else should fail
            }
          }
        }

        // need to reset globals after each new unification
        // do we need to make sure the unification is valid?
        if (unifyTerms(l1.termList, l2.termList) &&
          l1.termList == l2.termList) {
          unified = (newC1.deepCopy(), newC2.deepCopy()) +: unified
        }
        newC1 = c1.deepCopy()
        newC2 = c2.deepCopy()
        Term.usedTerms = oldBuffer.map(_.deepCopy())
      }

      i2 += 1
      if (i2 < newC2.literals.size) {
        unifyLiterals(newC1.literals(i1), newC2.literals(i2))
      } else {
        i1 += 1
        i2 = 0
        if (i1 < newC1.literals.size) {
          unifyLiterals(newC1.literals(i1), newC2.literals(i2))
        }
      }
    }
    unifyLiterals(newC1.literals(i1), newC2.literals(i2))

    if (unified.nonEmpty)
      Some(unified)
    else
      None
  }

  def main(args: Array[String]): Unit = {
    val c1 = parse(StdIn.readLine())
//    println(c1)
    val c2 = parse(StdIn.readLine())
//    println(c2 + "\n")

    val unified = apply(c1, c2)
    if (unified.isDefined) {
      for (u <- unified.get.reverse) {
        println(u._1)
        println(u._2)
        print("\n")
      }
    }
  }
}