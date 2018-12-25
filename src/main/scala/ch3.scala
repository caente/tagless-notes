package ch3

import ammonite.ops._
import simulacrum._

sealed trait Exp[Env, T]
case class B[Env]( b: Boolean ) extends Exp[Env, Boolean]
case class V[Env, T]( v: Var[Env, T] ) extends Exp[Env, T]
case class L[Env, A, B]( l: Exp[( A, Env ), B] ) extends Exp[Env, A => B]
case class A[Env, A, B]( f: Exp[Env, A => B], A: Exp[Env, A] ) extends Exp[Env, B]

sealed trait Var[+Env, +T]
case class VZ[Env, A]() extends Var[( A, Env ), A]
case class VS[Env, T, A]( x: Var[Env, T] ) extends Var[( A, Env ), T]

object initial {

  def eval[Env, T]( env: Env ): Exp[Env, T] => T = {
    case V( v ) => lookup( v, env )
    case B( b ) => b
    case L( e ) => x => eval( x, env )( e )
    case A( e1, e2 ) => eval( env )( e1 )( eval( env )( e2 ) )
  }

  def lookup[Env, T]( v: Var[Env, T], env: Env ): T =
    ( v, env ) match {
      case ( VZ(), ( x, _ ) )      => x
      case ( VS( v ), ( _, env ) ) => lookup( v, env )
    }

  def ti1[Env]: Exp[Env, Boolean] = A( L( V( VZ[Env, Boolean] ) ), B( true ) )

  println( "Initial" )
  println( "Expression: " + ti1[Unit] )
  println( "Evaluated:  " + eval( () )( ti1[Unit] ) )

}

object Symantics1 {
  trait Symantics[T[_, _]] {
    def int[H]( i: Int ): T[H, Int]
    def add[H]( e1: T[H, Int], e2: T[H, Int] ): T[H, Int]
    def z[H, A]: T[( A, H ), A]
    def s[H, A, any]( t: T[H, A] ): T[( any, H ), A]
    def lam[H, A, B]( t: T[( A, H ), B] ): T[H, A => B]
    def app[H, A, B]( f: T[H, A => B], a: T[H, A] ): T[H, B]
  }

  def td1[T[_, _], H]( implicit T: Symantics[T] ): T[H, Int] = {
    import T._
    add( int( 1 ), int( 2 ) )
  }

  def td2o[T[_, _], H]( implicit T: Symantics[T] ): T[( Int, H ), Int => Int] = {
    import T._
    lam( add( z[( Int, H ), Int], s( z[H, Int] ) ) )
  }

  def td3[T[_, _], H]( implicit T: Symantics[T] ): T[H, ( Int => Int ) => Int] = {
    import T._
    lam( add( app( z[H, Int => Int], int( 1 ) ), int( 2 ) ) )
  }

  //def error[T[_, _], H]( implicit T: Symantics[T] ) = {
  //  import T._
  //  lam( app( z[H, Int] )( z[H, Int] ) )
  //}

  {
    implicit object SymanticsR extends Symantics[R] {
      def int[H]( i: Int ): R[H, Int] = R( _ => i )
      def add[H]( e1: R[H, Int], e2: R[H, Int] ): R[H, Int] = R( h => e1.unR( h ) + e2.unR( h ) )
      def z[H, A]: R[( A, H ), A] = R( { case ( x, _ ) => x } )
      def s[H, A, any]( v: R[H, A] ): R[( any, H ), A] = R( { case ( _, h ) => v.unR( h ) } )
      def lam[H, A, B]( e: R[( A, H ), B] ): R[H, A => B] = R( h => x => e.unR( x, h ) )
      def app[H, A, B]( f: R[H, A => B], a: R[H, A] ): R[H, B] = R( h => f.unR( h )( a.unR( h ) ) )
    }

    case class R[H, A]( unR: H => A )
    def eval[A]( e: R[Unit, A] ) = e.unR( () )

    println( "eval td1: " + eval( td1 ) ) // 3

    //eval( td2o ) // won't compile because it's open

    println( "eval td3: " + eval( td3 ) )

    println( "eval td3 (_ + 2): " + eval( td3 )( _ + 1 ) ) // 4

  }

  {
    implicit object SymanticsS extends Symantics[S] {
      def int[H]( i: Int ): S[H, Int] = S( _ => i.toString )
      def add[H]( e1: S[H, Int], e2: S[H, Int] ): S[H, Int] = S( h => s"( ${e1.unS( h )} + ${e2.unS( h )} )" )
      def z[H, A]: S[( A, H ), A] = S( h => s"x${h - 1}" )
      def s[H, A, any]( t: S[H, A] ): S[( any, H ), A] = S( h => s"${t.unS( h - 1 )}" )
      def lam[H, A, B]( t: S[( A, H ), B] ): S[H, A => B] =
        S( h => {
          val x = s"x$h"
          s"( $x => ${t.unS( h + 1 )} )"
        } )
      def app[H, A, B]( f: S[H, A => B], a: S[H, A] ): S[H, B] =
        S( h =>
          s"( ${f.unS( h )} ${a.unS( h )} )" )
    }

    case class S[H, A]( unS: Int => String )
    def view[A]( e: S[Unit, A] ) = e.unS( 0 )

    println( "view td1: " + view( td1 ) ) // ( 1 + 2 )

    //view( td2o ) // won't compile because it's open

    println( "view td3: " + view( td3 ) ) // ( x0 => ( ( x0 1 ) + 2 ) )

  }

}

object Symantics2 {
  trait Symantics[T[_]] {
    def int( i: Int ): T[Int]
    def add( t1: T[Int], t2: T[Int] ): T[Int]
    def lam[A, B]( f: T[A] => T[B] ): T[A => B]
    def app[A, B]( f: T[A => B], a: T[A] ): T[B]
  }

  def th1[T[_]]( implicit T: Symantics[T] ): T[Int] = {
    import T._
    add( int( 1 ), int( 2 ) )
  }

  def th2[T[_]]( implicit T: Symantics[T] ): T[Int => Int] = {
    import T._
    lam( x => add( x, x ) )
  }

  def th3[T[_]]( implicit T: Symantics[T] ): T[( Int => Int ) => Int] = {
    import T._
    lam( ( x: T[Int => Int] ) => add( app( x, int( 1 ) ), int( 2 ) ) )
  }

  {
    case class R[A]( unR: A )
    implicit object SymanticsR extends Symantics[R] {
      def int( i: Int ): R[Int] = R( i )
      def add( t1: R[Int], t2: R[Int] ): R[Int] = R( t1.unR + t2.unR )
      def lam[A, B]( f: R[A] => R[B] ): R[A => B] = R( a => f( R( a ) ).unR )
      def app[A, B]( f: R[A => B], a: R[A] ): R[B] = R( f.unR( a.unR ) )
    }

    def eval[A]( r: R[A] ) = r.unR

    println( "eval th1: " + eval( th1 ) ) // 3

    println( "eval th2: " + eval( th2 )( 1 ) ) // 2

    println( "eval th3: " + eval( th3 ) )

    println( "eval th3 (_ + 2): " + eval( th3 )( _ + 2 ) ) // 5

  }

  {
    case class S[A]( unS: Int => String )
    implicit object SymanticsS extends Symantics[S] {
      def int( i: Int ): S[Int] = S( _ => i.toString )
      def add( e1: S[Int], e2: S[Int] ): S[Int] = S( h => s"( ${e1.unS( h )} + ${e2.unS( h )} )" )
      def lam[A, B]( f: S[A] => S[B] ): S[A => B] =
        S( h => {
          val x = s"x$h"
          s"( $x => ${f( S( _ => x ) ).unS( h + 1 )} )"
        } )
      def app[A, B]( f: S[A => B], a: S[A] ): S[B] =
        S( h =>
          s"( ${f.unS( h )} ${a.unS( h )} )" )
    }

    def view[A]( e: S[A] ) = e.unS( 0 )

    println( "view th1: " + view( th1 ) ) // ( 1 + 2 )

    println( "view th2: " + view( th2 ) ) // ( x0 => ( x0 + x0 ) )

    println( "view th3: " + view( th3 ) ) // ( x0 => ( ( x0 1 ) + 2 ) )

  }

}

