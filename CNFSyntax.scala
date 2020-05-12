import scala.collection.mutable.ArrayBuffer

package CNFSyntax {

  case class Clause(literals: List[MyLiteral]) {
    override def toString: String = literals.mkString(" | ")
    def update(): Unit = {
      for (l <- literals) {
        l.update()
      }
    }
    def deepCopy(): Clause = Clause(literals.map(_.deepCopy()))

    //  def change(v: Variable): Unit = {
    //    for (l <- literals ) {
    //      l match {
    //        case v: Variable => l
    //      }
    //    }
    //  }
  }

  case class MyLiteral(negated: Boolean, name: String, termList: ArrayBuffer[Term]) {
    override def toString: String = {
      if (negated) "-" else ""
    } + name + "(" + termList.mkString(", ") + ")"
    def update(): Unit = {
      updateTerms(termList)
      def updateTerms(list: ArrayBuffer[Term]): ArrayBuffer[Term] = {
        for (i <- list.indices) {
          val t = list(i)
          list(i) = t match {
            case Variable(_) => Term.usedTerms(t.index)
            case Function(n, tl) => Function(n, updateTerms(tl))
            case _ => t
          }
        }
        list
      }
    }
    def deepCopy(): MyLiteral = {
      MyLiteral(negated, name, termList.map(_.deepCopy()))
    }
  }


  abstract class Term(name: String) {
    var index: Int = 0
    val n: String = name
    override def toString: String = name
    def deepCopy(): Term
    def substitute(t: Term): Unit = {
      if (Term.usedTerms.size < index)
        throw new Exception("Trying to substitute value that doesn't exist")
      val test = Term.usedTerms(index)
      Term.usedTerms(index) = t
      if (t != test) {
        while (Term.usedTerms.contains(test)) {
          Term.usedTerms(Term.usedTerms.indexOf(test)) = t
        }
      }
    }
  }
  object Term {
    var usedTerms: ArrayBuffer[Term] = ArrayBuffer()
  }

  case class Constant(name: String) extends Term(name) {
    override def deepCopy(): Constant = {
      val newC = Constant(name)
      newC.index = index
      newC
    }
  }

  case class Function(name: String, termList: ArrayBuffer[Term]) extends Term(name) {
    override def toString: String = name + "(" + termList.mkString(", ") + ")"
    override def deepCopy(): Term = {
      val newF = Function(name, termList.map(_.deepCopy()))
      newF.index = index
      newF
    }
    def contains(v: String): Boolean = {
      var isContained = false
      for (t <- termList) {
        t match {
          case Variable(x) if x == v => isContained = true
          case f: Function => isContained = f.contains(v)
          case _ => Unit
        }
      }
      isContained
    }
  }

  case class Variable(name: String) extends Term(name) {
    override def deepCopy(): Term = {
      val newV = Variable(name)
      newV.index = index
      newV
    }
  }
  object Variable {
    def make(name: String): Variable = {
      Term.usedTerms.find(_.n == name) match {
        case Some(foundVar: Variable) => foundVar
        case _ =>
          val newVar = Variable(name)
          newVar.index = Term.usedTerms.size
          Term.usedTerms += newVar
          newVar
      }
    }
  }

}