object Arithmetic {
  def pow(b: Int, e: Int): Int = {
    if (e == 0) { 1 }
    else {
      if (e % 2 == 0) {
        val rec: Int = pow(b, e/2);
        rec * rec
      } else {
        b * pow(b, e - 1)
      }
    }
  }

  def gcd(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      a + b
    } else {
      if (a < b) {
        gcd(a, b % a)
      } else {
        gcd(a % b, b)
      }
    }
  }

  operator 2 def ? (a: Int): Int = {
    -!a
  }

  operator 1 def  & (a: Int, b : Int): Int = {
    a
  }

  operator 3 def @(a :Int, b :Int): Int = {
    a
  }
  a ? b & c @ d;
  a & b ? c
/*
  Std.printInt(pow(0, 10));
  Std.printInt(pow(1, 5));
  Std.printInt(pow(2, 10));
  Std.printInt(pow(3, 3));
  Std.printInt(gcd(0, 10));
  Std.printInt(gcd(17, 99)); // 1
  Std.printInt(gcd(16, 46)); // 2
  Std.printInt(gcd(222, 888)) // 222
*/
}
