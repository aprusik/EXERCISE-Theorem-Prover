import CNFSyntax._

import scala.collection.mutable.ArrayBuffer

object CNFParser {

  var varIncrement = 0

  /** Takes a string, returns a clause.
    *
    * @param s String
    * @return clause
    */
  def parse(s: String): Clause = {
    varIncrement += 1
    parseClause(s, 0)._1
  }

  /**  Tries to parse a clause starting from the index in the string.
    *
    * @param s String
    * @param i index of string
    * @return clause and the index after the last character consumed.
    */
  def parseClause(s: String, i: Int): (Clause, Int) = {
    var literals: List[MyLiteral] = Nil
    var n = i
    do {
      n = next(s, n)
      if (s.charAt(n) == '|') n = next(s, n+1)
      val literal = parseLiteral(s, next(s, n))
      literals = literal._1 +: literals
      n = literal._2
    } while (n < s.length && s.charAt(next(s, n)) == '|')
    (Clause(literals.reverse), n+1)  // reverse could slow things down
  }

  /** Tries to parse a literal starting at the index in the string.
    *
    * @param s String
    * @param i index of string
    * @return literal and the index after the last character consumed.
    */
  def parseLiteral(s: String, i: Int): (MyLiteral, Int) = {
    var negated = false
    var n = next(s, i)
    if (s.charAt( n ) == '-') {
      negated = true
      n = next(s, n+1)
    }
    val end = tokenEnd(s, n)
    s.slice(n, end) match {
      case name if name(0).isUpper =>
        val terms = parseTerms(s, end)
        (MyLiteral(negated, name, terms._1), terms._2)
      case _ => throw new Exception(
        "Error: Predicate doesn't start with capital letter.")
    }
  }

  /** Tries to parse a list of terms starting at the index in the string.
    *
    * @param s String
    * @param i index of string
    * @return terms and the index after the last character consumed.
    */
  def parseTerms(s: String, i: Int): (ArrayBuffer[Term], Int) = {
    var terms: ArrayBuffer[Term] = ArrayBuffer()
    var n = next(s, i)
    while (s.charAt(n) != ')' && s.charAt(next(s, n)) != ')') {
      val term = parseTerm(s, next(s, n+1))
      terms = terms :+ term._1
      n = term._2
    }
    (terms, n+1)  // reverse could slow things down
  }

  /** Tries to parse a single term starting at the index in the string.
    *
    * @param s String
    * @param i index of string
    * @return term and the index after the last character consumed.
    */
  def parseTerm(s: String, i: Int): (Term, Int) = {
    var end = tokenEnd(s, i)
    s.slice(i, end) match {
      case name if name(0).isUpper &&
        s.length > end &&
        s.charAt(end) == '(' =>
        val terms = parseTerms(s, end)
        (Function(name, terms._1), terms._2)
      case name if name(0).isUpper => (Constant(name), end)
      case name => (Variable.make(name + varIncrement), end)
    }
  }

  /**
    *
    * @param s String
    * @param i index of string
    * @return index of the first non-whitespace character at or after
    *         the index.
    */
  def next(s: String, i: Int): Int = {
    val c = s.indexWhere( !_.isWhitespace, i )
    if (s(c) == ',') next(s, c+1) else c
  }

  /**
    *
    * @param s String
    * @param i index of string
    * @return index of the first non-name character at or after
    *         the index.
    */
  def tokenEnd(s: String, i: Int): Int =
    s.indexWhere( !_.isLetterOrDigit, i ) match {
    case -1 => i + s.length
    case n => n
  }

  def main(args: Array[String]): Unit = {
    val s: String = "Mortal(x1,y,z) | Immortal(x1,Y(a,b,D(z)),z) | -R(x1,Popeye, z, F(z) )"

//    val foo = parse(s)
//    println(foo)
    val bar = parse(s)
//    bar.literals(2).termList.head.substitute(Constant("X"))
    bar.update()
    println(bar)
  }
}
