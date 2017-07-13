/**
  * FindRecursion class.
  * Uses T as a Stack Trace object to find repeating code parts
  */

class FindRecursion[T] {

  /**
    * Generalized version of substring.
    * @param input to extract the substring from
    * @param from starting position of the new string
    * @param to end position of the new string
    * @return the substring (see also String.substring)
    */
  def subString(input: Seq[T], from: Int, to: Int): Seq[T] = {
    input.dropRight(input.length - to).drop(from)
  }

  private def product(list: Seq[(Int, Any)]): Int = {
    list.foldLeft(1)((x, y) => x * y._1)
  }


  /**
    * counts prefixes for a given input
    * @param pattern is searched for how many times the sequence is started with it
    * @param input search base
    * @return the remaining values of the call chain and the number of times the pattern is recurring
    */
  def countPrefixes(pattern: Seq[T], input: Seq[T]): (Seq[T], Int) = {
    if (input.length < pattern.length) {
      (input, 0)
    } else {
      if (input.startsWith(pattern)) {
        val (rem, count) = countPrefixes(pattern, subString(input, pattern.length, input.length))
        (rem, count + 1)
      } else {
        (input, 0)
      }
    }
  }

  private def generatePrefixSequences(input: Seq[T]) = {
    (1 to input.length).map(i => {
      val matching = subString(input, 0, i)
      val (remaining, count) = countPrefixes(matching, input)
      Seq((count, matching)) ++ findRecursion(remaining)
    })
  }

  private def extractMaximalCompression(list: Seq[Seq[(Int, Seq[T])]]) = {
    list.foldLeft((0, Seq[(Int, Seq[T])]()))((max, actual: Seq[(Int, Seq[T])]) => {
      val value = product(actual)
      if (max._1 <= value) {
        (value, actual)
      } else {
        max
      }
    })._2
  }
  /**
    * finds recurring elements in a call chain
    * @param input the call chain
    * @return compressed view of call chain with number of repetitions and repeating part sequences
    */
  def findRecursion(input: Seq[T]): Seq[(Int, Seq[T])] = {
    input match {
      case Seq() => Seq()
      case _ =>
        extractMaximalCompression(generatePrefixSequences(input))
    }
  }

}
