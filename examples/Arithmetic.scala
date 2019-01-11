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

	operator 100 def **(x: Int, y: Int): Int = {
		pow(x, y)
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
	operator 5 def ^(x: Int, y: Int): Int = {
		gcd(x, y)
	}
	operator 3 def <<(x: Int, y: Int): Int = {
		if(y == 0) {
			x
		}
		else {
			2 * x << y - 1
		}
	}
	Std.printString("Should be 4:");
	Std.printInt(2 << 1 ** 2)
}
