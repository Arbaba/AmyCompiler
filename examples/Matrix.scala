object Matrix {
	abstract class Matrix
	case class Vector2(x: Int, y: Int) extends Matrix
	/**
(a b)
(c d)
	*/
	case class Matrix2x2(a: Int, b: Int, c: Int, d: Int) extends Matrix

//numpy-like dot product associative, non-commutative operator
	operator 80 def @(w: Matrix2x2, x: Matrix): Matrix = {
		w match {
			case Matrix2x2(a, b, c, d) => {
				x match {
					case Vector2(x, y) => Vector2(x*a + y*b, x*c + y*d)
					case Matrix2x2(r, s, t, u) => Matrix2x2(
						a*r + b*t, a*s + b*u,
						c*r + d*t, c*s + d*u
					)
				}
			}
		}
	}
}
