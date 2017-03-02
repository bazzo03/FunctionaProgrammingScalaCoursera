object Exercise {
  def times(chars: List[Char]): List[(Char, Int)] = {
    @annotation.tailrec
    def loop(list: List[Char], finalList : List[(Char, Int)], pos : Int) : List[(Char, Int)] = {
      list match {
        case Nil => finalList
        case x :: xs => //verifies if the char exists already : if yes -> add one to the count - if not -> adds the char
          if (validateExistance(list, list(pos)) != -1) {
            val list2:List[(Char, Int)] = List[(Char, Int)](( list(pos), finalList(pos)._2 + 1))
            loop (xs, finalList ++ list2 , pos - 1)
          } else {
            val list2:List[(Char, Int)] = List[(Char, Int)] ((list(pos), 1))
            loop (xs, finalList ++ list2, pos - 1)
          }
      }
    }
    loop(chars, List(), chars.size - 1)
  }

  def validateExistance(list: List[Char], char : Char ) : Int = {
    //    @annotation.tailrec
    def loop (n : Int, list: List[Char], char : Char): Int = {
      list match {
        case Nil => -1
        case x :: xs =>
          if (list.head != char) {
            loop (n-1, list.tail, char)
          }
          else{
            n
          }
      }
    }

    loop (list.size, list, char)
  }


  print(times(List('a','b','c','a','d','e')))
}