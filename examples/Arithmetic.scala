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

	/*operator 50 def <<(x: Int, y: Int): Int = {
		if(y == 0) {
			x
		}
		else {
			2 * x << y - 1
		}
	}*/

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
	//Std.printInt(pow(0, 10))
  Std.printInt(1**5);
  Std.printInt(2**10);
  Std.printInt(3**3);
  Std.printInt(0**10);
  Std.printInt(17 ^ 99); // 1
  Std.printInt(16 ^ 46); // 2
  Std.printInt(222 ^ 888);
	Std.printInt(3 ^ 7**2 * 3);
	Std.printInt(7 ^ 8);

	Std.printInt(2*3*5 ^ (2*3)**2 % 5)
	//gcd()

}
