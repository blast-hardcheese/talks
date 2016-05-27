trait ApplicativeExt { self: Predef =>

	implicit class Lift6Applicative[F[_]](context: Applicative[F]) {
		import context.{ wrap, call }

		def lift6[A1, A2, A3, A4, A5, A6, Result](f: (A1, A2, A3, A4, A5, A6) => Result): (F[A1], F[A2], F[A3], F[A4], F[A5], F[A6]) => F[Result] = {
			case (fa1, fa2, fa3, fa4, fa5, fa6) =>
				call(fa6)(call(fa5)(call(fa4)(call(fa3)(call(fa2)(call(fa1)(wrap(f.curried)))))))
		}
	}
}
