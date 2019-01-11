object Matrix {
	abstract class Matrix
	case class Vector2(x: Int, y: Int) extends Matrix
	/**
(a b)
(c d)
	*/
	case class Matrix2x2(a: Int, b: Int, c: Int, d: Int) extends Matrix

//numpy-like dot product associative, non-commutative operator
	operator 80 def @(w: Matrix2x2, v: Matrix): Matrix = {
		w match {
			case Matrix2x2(a, b, c, d) =>
				v match {
					case Vector2(x, y) => Vector2(x*a + y*b, x*c + y*d)
					case Matrix2x2(r, s, t, u) => Matrix2x2(
						a*r + b*t, a*s + b*u,
						c*r + d*t, c*s + d*u
					)
				}
		}
	}
	abstract class Tuple
	case class Pair(a: Int, b: Int) extends Tuple
	operator 99 def ::(a: Int, b: Int): Pair = {//lisp-like cons
		Pair(a, b)
	}

	val I: Matrix = Matrix2x2(1, 0, 0, 1);
	//val swap: Matrix2x2 = Matrix2x2(0, 1, 1, 0);
	val W: Matrix2x2 = Matrix2x2(53, 36, 24, 34);
	val x: Vector2 = Vector2(5, 3);
	W @ x
}
