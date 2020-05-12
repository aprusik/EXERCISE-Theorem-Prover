import CNFParser.parse
import CNFSyntax.Clause

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

object Prove {
  val steps: mutable.Map[Int, (Int, Int, Clause)] = mutable.Map.empty // step#, (kbStep#, SoSStep#, Clause)
  var last: (Int, Int, Int) = (0,0,0) // (KBStep#, SoSStep#, Step#)
  var totalResolutions = 0

  def apply(kb: Set[(Clause, Int)], nq: Clause): Boolean = {
    steps.clear()
    var newKB: Set[Clause] = Set.empty
    val clauseList = kb.unzip._1
    val _sos = Set[Clause](nq)
    var index = kb.toMap + {(_sos.head, kb.size)}

    @tailrec
    def helper(clauses: Set[Clause], sos: Set[Clause]): Boolean = {
      var n = index.size+1

      if (clauses.exists(ci => sos.exists( cj => {
        val i = index(ci) + 1
        val j = index(cj) + 1
        resolve(ci, cj) match {
          case Some(cl) if cl.exists(x => sos(x) || newKB(x)) => false
          case Some(cl) if cl.exists(_.literals.isEmpty) =>
            totalResolutions += 1
//            println(s"$i and $j give $n: <empty>")
            last = (i, j, n)
            true
          case Some(cl) =>
            cl.foreach( x => {
              index = index + {(x, n-1)}
              totalResolutions += 1
//              println(s"$i and $j give $n: $x")
              steps(n) = (i, j, x)
            })
            n += 1
            newKB = newKB ++ cl
            false
          case _ => false
        }
      })))
        true
      else {
        if (newKB.subsetOf(sos)) false
        else helper(clauses, sos ++ newKB)
      }

    }
    helper(clauseList, _sos)
  }

  def resolve(ci: Clause, cj: Clause): Option[List[Clause]] = {
    val u = Unify(ci, cj)
    if (u.isDefined) {
      Some (
        for (clauses <- u.get) yield {
          var newClause: Clause = Clause(Nil)
          // for each pair l1 and l2 in c1 and c2, return first true, else false
          if (clauses._1.literals exists (l1 => clauses._2.literals exists (l2 =>
            if (
              l1.name == l2.name &&
              l1.termList == l2.termList &&
              l1.negated != l2.negated
            ) {
              val newCL1 = clauses._1.literals.toSet - l1
              val newCL2 = clauses._2.literals.toSet - l2
              newClause = Clause((newCL1 ++ newCL2).toList)
              true
            } else false
          ))) newClause
          else
            throw new Exception("Error: Invalid unification passed to resolve.")
        }
      )
    } else None
  }

  def printSteps(proven: Boolean): Unit = {
    var stepList: List[(Int, Int, Int, Clause)] = Nil
    val numbers: mutable.Map[Int, Int] = mutable.Map.empty

    var i = last._2
    while (steps.contains(i)) {
      val n = steps(i)
      stepList = (n._1, n._2, i, n._3) +: stepList
      i = n._2
    }

    stepList.foreach({case (x, y, z, c) =>
      i += 1
      numbers(z) = i
      val y2 = {if (numbers.contains(y)) numbers(y) else y}
      println(s"$x and $y2 give $i: $c")})
    val lx = last._1
    val li = {if (numbers.contains(last._2)) numbers(last._2) else last._2}
    i += 1
    if (proven) println(s"$lx and $li give $i: <empty>")

  }

  def main(args: Array[String]): Unit = {
    var text = StdIn.readLine()
    var _kb: Array[Clause] = Array.empty
    while (text != "--- negated query ---") {
      _kb = _kb :+ parse(text)
      text = StdIn.readLine()
    }
    val nq = parse(StdIn.readLine())
    _kb.foreach(c => println(_kb.indexOf(c)+1 + s": $c") )
    println(_kb.length+1 + s": $nq")

    val proven = Prove(_kb.zipWithIndex.toSet, nq)
    printSteps(proven)
    if (!proven)
      println("No proof exists.")
    println(s"$totalResolutions total resolutions")
//    val resolution = resolve(_kb(0), _kb(4))
//    val i = _kb.size + 1
//    resolution.get.foreach(c => println(s"1 and 5 give $i: $c"))
  }
}
