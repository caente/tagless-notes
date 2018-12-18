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

object tagless extends App {
  trait Symantics[T[_, _]] {
    def int[H]( i: Int ): T[H, Int]
    def add[H]( e1: T[H, Int], e2: T[H, Int] ): T[H, Int]
    def z[H, A]: T[( A, H ), A]
    def s[H, A, Z]( t: T[H, A] ): T[( Z, H ), A]
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

  implicit object SymanticsR extends Symantics[R] {
    def int[H]( i: Int ): R[H, Int] = R( _ => i )
    def add[H]( e1: R[H, Int], e2: R[H, Int] ): R[H, Int] = R( h => e1.unR( h ) + e2.unR( h ) )
    def z[H, A]: R[( A, H ), A] = R( { case ( x, _ ) => x } )
    def s[H, A, Z]( v: R[H, A] ): R[( Z, H ), A] = R( { case ( _, h ) => v.unR( h ) } )
    def lam[H, A, B]( e: R[( A, H ), B] ): R[H, A => B] = R( h => x => e.unR( x, h ) )
    def app[H, A, B]( f: R[H, A => B], a: R[H, A] ): R[H, B] = R( h => f.unR( h )( a.unR( h ) ) )
  }

  case class R[H, A]( unR: H => A )
  def eval[A]( e: R[Unit, A] ) = e.unR( () )

  println( "eval td1: " + eval( td1 ) )

  //eval( td2o ) // won't compile because it's open

  println( "eval td3: " + eval( td3 ) )

  println( "eval td3 (_ + 2): " + eval( td3 )( _ + 1 ) )

}

