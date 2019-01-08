object Operators {
	operator 100 def **(x: Int, y: Int): Int = {
		Arithmetic.pow(x, y)
	}

	operator 50 def <<(x: Int, y: Int): Int = {
		if(y == 0) {
			x
		}
		else {
			2 * x << y - 1
		}
	}
	operator 20 def ^(x: Boolean, y: Boolean): Boolean = {
		if(x == true) {
			!y
		}
		else {
			y
		}
	}

	operator 50 def >>(x: Int, y: Int): Int = {
		if(y == 0) {
			x
		}
		else {
			x / 2 << y - 1
		}
	}

	error("1")
}
