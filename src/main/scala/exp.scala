package exp

object tagged {

  import shapeless._
  sealed trait Number
  case object One extends Number
  case object Two extends Number
  case object Three extends Number

  def find( i: Int ): Option[Number] =
    i match {
      case 1 => Some( One )
      case 2 => Some( Two )
      case 3 => Some( Three )
      case _ => None
    }

  trait FindNumber {
    type Out
    def find( i: Int ): Option[Out]
  }
  object FindNumber {
    type Aux[T] = FindNumber { type Out = T }
    def apply[T]( implicit T: Aux[T] ) = T
    implicit def coproduct[H, T <: Coproduct](
      implicit
      h: Aux[H],
      t: Aux[T],
      b: ops.coproduct.Basis[H :+: T, T],
      i: ops.coproduct.Inject[H :+: T, T]
    ): Aux[H :+: T] = new FindNumber {
      type Out = H :+: T
      def find( i: Int ): Option[Out] =
        h.find( i ).map( e => Coproduct[H :+: T]( e ) )
          .orElse( t.find( i ).map( _.embed[H :+: T] ) )
    }
    implicit object number extends FindNumber {
      type Out = Number
      def find( i: Int ) = ???
    }
  }
}

object tagged2 extends App {
  case class Number[A]( s: A, next: Number[A] )

  object Number {
    def create[A]( current: A )( next: Number[A] ): Number[A] = ???
  }
}

object tagless {
  import simulacrum._
  import scalaz._, Scalaz._
  @typeclass trait Number[F[_]] {
    def one( i: Int ): F[Int]
    def two( i: Int ): F[Int]
    def three( i: Int ): F[Int]
  }

  @typeclass trait OrElse[F[_]] {
    def orElse[A]( x: F[A] ): F[A] => F[A]
  }

  case class S[A]( s: Option[String] )
  object OrElse {

    implicit object OrElseOption extends OrElse[Option] {
      def orElse[A]( x: Option[A] ) =
        y => x.orElse( y )
    }

    implicit object OrElseS extends OrElse[S] {
      def orElse[A]( x: S[A] ) =
        y => S( x.s.map( x => s"Some($x)" ).orElse( Some( s"None.orElse(${y.s})" ) ) )
    }
  }

  import OrElse.ops._
  import Number.ops._

  def numbers[F[_]]( i: Int )( implicit N: Number[F], O: OrElse[F] ) =
    N.one( i ).orElse( N.two( i ) ).orElse( N.three( i ) )

  object Number {
    implicit object NumberOpt extends Number[Option] {
      def one( i: Int ) = ( i === 1 ).option( i )
      def two( i: Int ) = ( i === 2 ).option( i )
      def three( i: Int ) = ( i === 3 ).option( i )
    }
    implicit object NumberS extends Number[S] {
      def one( i: Int ) = S( ( i === 1 ).option( i.toString ) )
      def two( i: Int ) = S( ( i === 2 ).option( i.toString ) )
      def three( i: Int ) = S( ( i === 3 ).option( i.toString ) )

    }
  }

  println( numbers[Option]( 1 ) ) // Some(3)
  println( numbers[Option]( 4 ) ) // None
  println( numbers[S]( 3 ).s ) // Some(3)
  println( numbers[S]( 4 ).s ) //
}

