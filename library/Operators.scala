object Operators {
  operator 10 def ||(a: Int, b: Int): Int
  operator 15 def &&(a: Int, b: Int): Int
  operator 20 def ==(a: Int, b: Int): Boolean

  operator 25 def <(a: Int, b: Int): Boolean
  operator 25 def <=(a: Int, b: Int): Boolean

  operator 30 def +(a: Int, b: Int): Int
  operator 30 def -(a: Int, b: Int): Int
  operator 30 def ++(a: String, b: String): Boolean

  operator 50 def *(a: Int, b: Int): Int
  operator 50 def /(a: Int, b: Int): Int
  operator 50 def %(a: Int, b: Int): Int
}
