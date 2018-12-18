package ch2

import ammonite.ops._
import simulacrum._

sealed trait Exp
case class Lit( e: Int ) extends Exp
case class Neg( e: Exp ) extends Exp
case class Add( e1: Exp, e2: Exp ) extends Exp
//added
case class Mul( e1: Exp, e2: Exp ) extends Exp

object main {
  //8 + ( -( 1 + 2 ) )
  val ti1 = Add( Lit( 8 ), Neg( Add( Lit( 1 ), Lit( 2 ) ) ) )

  val eval: Exp => Int = {
    case Lit( n )      => n
    case Neg( e )      => -eval( e )
    case Add( e1, e2 ) => eval( e1 ) + eval( e2 )
    //added
    case Mul( e1, e2 ) => eval( e1 ) * eval( e2 )
  }

  //println(s"eval ti1 = ${eval(ti1)}")

  //type Repr = Int
  //val lit: Repr => Int = n => n
  //val neg: Repr => Repr = n => -n
  //val add: Repr => Repr => Repr = n1 => n2 => n1 + n2

  //val tf1 = add(lit(8))( neg(add(lit(1))(lit(2))))

  //println(s"eval tf1 = ${tf1}")

  def view( e: Exp ): String =
    e match {
      case Lit( n )      => n.toString
      case Neg( e )      => s"(-${view( e )})"
      case Add( e1, e2 ) => s"(${view( e1 )} + ${view( e2 )})"
      //added
      case Mul( e1, e2 ) => s"(${view( e1 )} * ${view( e2 )})"
    }

  //println(s"ti1:=${view(ti1)}")

  @typeclass trait ExpSYM[T] {
    def lit( n: Int ): T
    def neg( n: T ): T
    def add( n1: T, n2: T ): T
  }

  import ExpSYM.ops._

  implicit object ExpSYMInt extends ExpSYM[Int] {
    def lit( n: Int ): Int = n
    def neg( n: Int ): Int = -n
    def add( n1: Int, n2: Int ): Int = n1 + n2
  }

  implicit object ExpSYMString extends ExpSYM[String] {
    def lit( n: Int ): String = n.toString
    def neg( n: String ): String = s"-$n"
    def add( n1: String, n2: String ): String = s"($n1 + $n2)"
  }

  //added
  @typeclass trait MulSYM[T] {
    def mul( n1: T, n2: T ): T
  }
  implicit object MulSYMInt extends MulSYM[Int] {
    def mul( n1: Int, n2: Int ): Int = n1 + n2
  }
  implicit object MulSYMString extends MulSYM[String] {
    def mul( n1: String, n2: String ): String = s"($n1 * $n2)"
  }
  import MulSYM.ops._
  //added

  def tf1[T]( implicit T: ExpSYM[T] ): T = {
    import T._
    add( lit( 8 ), neg( add( lit( 1 ), lit( 2 ) ) ) )
  }

  def tf2[T]( implicit T: ExpSYM[T], M: MulSYM[T] ): T = {
    import T._
    import M._
    val inner = neg( add( lit( 1 ), lit( 2 ) ) )
    neg( mul( add( lit( 8 ), inner ), lit( 9 ) ) )
  }

  //need an special context to compose two final "objects"
  def tf3[T]( implicit T: ExpSYM[T] ): T = {
    import T._
    add( tf1[T], lit( 8 ) )
  }

  //2.3
  sealed trait Tree
  case class Leaf( s: String ) extends Tree
  case class Node( s: String, trees: List[Tree] ) extends Tree

  implicit object ExpSYMTree extends ExpSYM[Tree] {
    def lit( n: Int ): Tree = Leaf( n.toString )
    def neg( n: Tree ): Tree = Node( "-", List( n ) )
    def add( n1: Tree, n2: Tree ): Tree = Node( "+", List( n1, n2 ) )
  }
  implicit object MulSYMTree extends MulSYM[Tree] {
    def mul( n1: Tree, n2: Tree ): Tree = Node( "*", List( n1, n2 ) )
  }
  //2.3

  sealed trait Ctx
  object Ctx {
    case object Pos extends Ctx
    case object Neg extends Ctx
  }
  implicit def ExpSYMCtx[T]( implicit E: ExpSYM[T] ) = new ExpSYM[Ctx => T] {
    def lit( n: Int ): Ctx => T = {
      case Ctx.Pos => E.lit( n )
      case Ctx.Neg => E.neg( E.lit( n ) )
    }
    def neg( n: Ctx => T ): Ctx => T = {
      case Ctx.Pos => n( Ctx.Neg )
      case Ctx.Neg => n( Ctx.Pos )
    }
    def add( n1: Ctx => T, n2: Ctx => T ): Ctx => T =
      ctx => E.add( n1( ctx ), n2( ctx ) )
  }
  implicit def MulSYMCtx[T]( implicit E: MulSYM[T] ) = new MulSYM[Ctx => T] {
    def mul( n1: Ctx => T, n2: Ctx => T ): Ctx => T = {
      case Ctx.Pos => E.mul( n1( Ctx.Pos ), n2( Ctx.Pos ) )
      case Ctx.Neg => E.mul( n1( Ctx.Pos ), n2( Ctx.Neg ) )
    }
  }

  def push_negI( e: Exp ): Exp =
    e match {
      case e @ Lit( _ )         => e
      case e @ Neg( Lit( _ ) )  => e
      case Neg( Neg( e ) )      => push_negI( e )
      case Neg( Add( e1, e2 ) ) => Add( push_negI( Neg( e1 ) ), push_negI( Neg( e2 ) ) )
      case Add( e1, e2 )        => Add( push_negI( e1 ), push_negI( e2 ) )
    }
  def flataI( e: Exp ): Exp =
    e match {
      case e @ Lit( _ )             => e
      case e @ Neg( _ )             => e
      case Add( Add( e1, e2 ), e3 ) => flataI( Add( e1, Add( e2, e3 ) ) )
      case Add( e1, e2 )            => Add( e1, flataI( e2 ) )
    }
  def normI( e: Exp ): Exp = e |> push_negI |> flataI

  val ti2 = Add( ti1, Lit( 8 ) )
  println( "Initial" )
  println( "view ti2:" + ( ti2 |> view ) )
  println( "push_neg ti2:" + ( ti2 |> push_negI |> view ) )
  println( "view flatten ti2:" + ( ti2 |> flataI |> view ) )
  println( "view normI ti2:" + ( ti2 |> normI |> view ) )
  /*
  Initial
  view ti2:((8 + (-(1 + 2))) + 8)
  push_neg ti2:((8 + ((-1) + (-2))) + 8)
  view flatten ti2:(8 + ((-(1 + 2)) + 8))
  view normI ti2:(8 + ((-1) + ((-2) + 8)))
  */

  sealed trait CtxL[+A]
  case class LCA[A]( a: A ) extends CtxL[A]
  case object NonLCA extends CtxL[Nothing]

  implicit def ExpSYMCtxL[T]( implicit T: ExpSYM[T] ) = new ExpSYM[CtxL[T] => T] {
    def lit( n: Int ): CtxL[T] => T = {
      case NonLCA   => T.lit( n )
      case LCA( e ) => T.add( T.lit( n ), e )
    }
    def neg( n: CtxL[T] => T ): CtxL[T] => T = {
      case NonLCA   => T.neg( n( NonLCA ) )
      case LCA( e ) => T.add( T.neg( n( NonLCA ) ), e )
    }
    def add( n1: CtxL[T] => T, n2: CtxL[T] => T ): CtxL[T] => T = ctx => n1( LCA( n2( ctx ) ) )
  }

  implicit def MulSYMCtxL[T]( implicit T: MulSYM[T] ) = new MulSYM[CtxL[T] => T] {
    def mul( n1: CtxL[T] => T, n2: CtxL[T] => T ): CtxL[T] => T = ctx => n1( LCA( n2( ctx ) ) )
  }

  type Signed[A] = Ctx => A
  type Balanced[T] = CtxL[T] => T
  type Normalized[T] = Signed[Balanced[T]]

  def push_negF[T]( e: Signed[T] ): T = e( Ctx.Pos )
  def flataF[T]( e: Balanced[T] ): T = e( NonLCA )
  def normF[T]( e: Normalized[T] ): T = e |> push_negF |> flataF

  println( "Final" )
  println( "view tf3:" + tf3[String] )
  println( "push_neg tf3:" + ( tf3[Signed[String]] |> push_negF ) )
  println( "view flatten tf3:" + ( tf3[Balanced[String]] |> flataF ) )
  println( "view norm tf3:" + ( tf3[Normalized[String]] |> normF ) )
  /*
  Final
  view tf3:((8 + -(1 + 2)) + 8)
  push_neg tf3:((8 + (-1 + -2)) + 8)
  view flatten tf3:(8 + (-(1 + 2) + 8))
  view norm tf3:(8 + (-1 + (-2 + 8)))
  */
}
