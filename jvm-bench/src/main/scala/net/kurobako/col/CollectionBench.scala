package net.kurobako.col

import java.util
import java.util.concurrent.TimeUnit

import cats.data.Chain
import chimera.compiler.datastructures.SlidingBuffer
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.util.Random


final object CollectionBench {

	case class Fixture[A](seed: Int, actual: Seq[A], a: A, aIdx: Int) {
	}

	trait MutableOps {
		type A
		type C[_]
		def append: C[A]
		def prepend: C[A]
		def concat: C[A]
		def drainArray: Array[A]
		def index: A
		def apply: C[A]
		def applyAll: C[A]
		def size: Int

		def _collapse: C[A] => Seq[A]
		def _fixture: Fixture[A]
	}

	//noinspection TypeAnnotation
	@BenchmarkMode(Array(Mode.AverageTime))
	@OutputTimeUnit(TimeUnit.NANOSECONDS)
	@State(Scope.Benchmark)
	class Mutable {
		@Benchmark def appendOne(input: MutableInput) = input.ops.append
		@Benchmark def prependOne(input: MutableInput) = input.ops.prepend
		@Benchmark def concat(input: MutableInput) = input.ops.concat
		@Benchmark def drainToArray(input: MutableInput) = input.ops.drainArray
		@Benchmark def index(input: MutableInput) = input.ops.index
		@Benchmark def apply(input: MutableInput) = input.ops.apply
		@Benchmark def applyAll(input: MutableInput) = input.ops.applyAll
		@Benchmark def size(input: MutableInput) = input.ops.size
	}

	trait ImmutableOps {
		type A
		type C[_]
		def head: Option[A]
		def tail: C[A]
		def append: C[A]
		def prepend: C[A]
		def concat: C[A]
		def drainArray: Array[A]
		def apply: C[A]
		def applyAll: C[A]
		def foldL: Int
		def size: Int

		def _collapse: C[A] => Seq[A]
		def _fixture: Fixture[A]
	}

	//noinspection TypeAnnotation
	@BenchmarkMode(Array(Mode.AverageTime))
	@OutputTimeUnit(TimeUnit.NANOSECONDS)
	@State(Scope.Benchmark)
	class Immutable {
		@Benchmark def head(input: ImmutableInput) = input.ops.head
		@Benchmark def tail(input: ImmutableInput) = input.ops.tail
		@Benchmark def append(input: ImmutableInput) = input.ops.append
		@Benchmark def prepend(input: ImmutableInput) = input.ops.prepend
		@Benchmark def concat(input: ImmutableInput) = input.ops.concat
		@Benchmark def drainToArray(input: ImmutableInput) = input.ops.drainArray
		@Benchmark def apply(input: ImmutableInput) = input.ops.apply
		@Benchmark def applyAll(input: ImmutableInput) = input.ops.applyAll
		@Benchmark def foldLHashCode(input: ImmutableInput) = input.ops.foldL
		@Benchmark def size(input: ImmutableInput) = input.ops.size
	}

	final val StringTpe = "String"
	final val IntTpe    = "Int"

	final val JavaArrayList        = "java.util.ArrayList"
	final val JavaLinkedList       = "java.util.LinkedList"
	final val ScalaListBuffer      = "mutable.ListBuffer"
	final val ScalaArrayBuffer     = "mutable.ArrayBuffer"
	final val ChimeraSlidingBuffer = "chimera.SlidingBuffer"

	final val ScalaList       = "List"
	final val ScalaVector     = "Vector"
	final val ScalaStream     = "Stream"
	final val JavaArray       = "Array"
	final val CatsChainList   = "Chain(List)"
	final val CatsChainVector = "Chain(Vector)"
	final val CatsChainStream = "Chain(Stream)"
	final val CatsChainArray  = "Chain(Array)"

	private def prepare[A, B](size: Int, tpe: String)(implicit
													  strMkOp: Fixture[String] => B,
													  intMkOp: Fixture[Int] => B): B = {
		val mid = size / 2
		val data = 0 to size
		tpe match {
			case StringTpe =>
				val xs = Random.shuffle(data.map(_.toString)).toVector
				strMkOp(Fixture(1, xs, xs(mid), mid))
			case IntTpe    =>
				val xs = Random.shuffle(data).toVector
				intMkOp(Fixture(1, xs, xs(mid), mid))
			case bad       => sys.error(s"bad element type `$bad`")
		}
	}

	@State(Scope.Thread) class MutableInput {

		@Param(Array(
			//			"1",
			//			"10",
			//			"100",
			"1000",
//			"10000",
			"100000",
			//			"1000000",
			//			"10000000",
		))
		var size: Int = _

		@Param(Array(StringTpe, IntTpe))
		var elementTpe: String = _

		@Param(Array(JavaArrayList, JavaLinkedList, ScalaListBuffer, ScalaArrayBuffer))
		var collection: String = _

		var ops: MutableOps = _

		implicit def prepareMutable[X: ClassTag](fixture: Fixture[X]): MutableOps = {
			import scala.collection.JavaConverters._
			val xs = fixture.actual
			val x = fixture.a
			val mid = fixture.aIdx
			collection match {
				case ScalaArrayBuffer     => val cs = ArrayBuffer(xs: _*)
					new MutableOps {
						type A = X
						type C[x] = ArrayBuffer[x]
						def append = {cs append x; cs}
						def prepend = {cs prepend x; cs}
						def concat = {cs appendAll cs; cs}

						def drainArray = cs.toArray
						def index = cs(mid)
						def apply = ArrayBuffer()
						def applyAll = ArrayBuffer(xs: _*)
						def size: Int = cs.length
						val _fixture  = fixture
						val _collapse = _.toSeq
					}
				case ScalaListBuffer      => val cs = ListBuffer(xs: _*)
					new MutableOps {
						type A = X
						type C[x] = ListBuffer[x]
						def append = {cs append x; cs}
						def prepend = {cs prepend x; cs}
						def concat = {cs appendAll cs; cs}
						def drainArray = cs.toArray
						def index = cs(mid)
						def apply = ListBuffer()
						def applyAll = ListBuffer(xs: _*)
						def size: Int = cs.length
						val _fixture  = fixture
						val _collapse = _.toSeq
					}
				case JavaArrayList        =>
					val jc = xs.asJavaCollection
					val cs = new util.ArrayList[X](jc)
					new MutableOps {
						type A = X
						type C[x] = util.ArrayList[x]
						def append = {cs add x; cs}
						def prepend = {cs add(0, x); cs}
						def concat = {cs addAll cs; cs}
						def drainArray = cs.toArray.asInstanceOf[Array[A]]
						def index = cs.get(mid)
						def apply = new util.ArrayList[A]()
						def applyAll = new util.ArrayList[A](jc)
						def size: Int = cs.size()
						val _fixture  = fixture
						val _collapse = _.asScala.toSeq
					}
				case JavaLinkedList       =>
					val jc = xs.asJavaCollection
					val cs = new util.LinkedList[X](jc)
					new MutableOps {
						type A = X
						type C[x] = util.LinkedList[x]
						def append = {cs add x; cs}
						def prepend = {cs add(0, x); cs}
						def concat = {cs addAll cs; cs}
						def drainArray = cs.toArray.asInstanceOf[Array[A]]
						def index = cs.get(mid)
						def apply = new util.LinkedList[A]()
						def applyAll = new util.LinkedList[A](jc)
						def size: Int = cs.size()
						val _fixture  = fixture
						val _collapse = _.asScala.toSeq
					}
				case ChimeraSlidingBuffer =>
					val cs = SlidingBuffer(xs: _*)
					new MutableOps {
						type A = X
						type C[x] = SlidingBuffer[x]
						def append = {cs append x; cs}
						def prepend = {cs prepend x; cs}
						def concat = {cs appendAll cs; cs}
						def drainArray = cs.toArray
						def index = cs(mid)
						def apply = SlidingBuffer()
						def applyAll = SlidingBuffer(xs: _*)
						def size: Int = cs.length
						val _fixture  = fixture
						val _collapse = _.toSeq
					}

				case bad => sys.error(s"bad collection type `$bad`")

			}
		}


		@Setup(Level.Iteration) def setup(): Unit = ops = prepare(size, elementTpe)

	}





	@State(Scope.Thread) class ImmutableInput {

		@Param(Array(
			//			"1",
			//			"10",
			//			"100",
			"1000",
//			"10000",
			"100000",
			//			"1000000",
			//			"10000000",
		))
		var size: Int = _

		@Param(Array(StringTpe, IntTpe))
		var elementTpe: String = _

		@Param(Array(ScalaList, ScalaVector, ScalaStream, JavaArray,
			CatsChainList, CatsChainVector, CatsChainStream, CatsChainArray))
		var collection: String = _

		var ops: ImmutableOps = _

		private implicit def prepareImmutable[X: ClassTag](fixture: Fixture[X]): ImmutableOps = {
			val xs = fixture.actual
			val x = fixture.a


			@inline def mkChainOps(xs: Seq[X]) = {
				val cs = Chain.fromSeq(xs)
				new ImmutableOps {
					type A = X
					type C[x] = Chain[x]
					def head = cs.headOption
					def tail = cs.uncons.get._2
					def append = cs :+ x
					def prepend = x +: cs
					def concat = cs ++ cs
					def drainArray = cs.iterator.toArray
					def apply = Chain.empty
					def applyAll = Chain(xs: _*)
					def foldL = cs.foldLeft(0)(_.hashCode + _.hashCode)
					def size = cs.length.toInt
					val _fixture  = fixture
					val _collapse = _.toList
				}
			}

			collection match {
				case ScalaList   => val cs = xs.toList
					new ImmutableOps {
						type A = X
						type C[x] = List[x]
						def head = cs.headOption
						def tail = cs.tail
						def append = cs :+ x
						def prepend = x +: cs
						def concat = cs ++ cs
						def drainArray = cs.toArray
						def apply = List.empty
						def applyAll = List(xs: _*)
						def foldL = cs.foldLeft(0)(_.hashCode + _.hashCode)
						def size = cs.size
						val _fixture  = fixture
						val _collapse = _.toSeq
					}
				case ScalaVector => val cs = xs.toVector
					new ImmutableOps {
						type A = X
						type C[x] = Vector[x]
						def head = cs.headOption
						def tail = cs.tail
						def append = cs :+ x
						def prepend = x +: cs
						def concat = cs ++ cs
						def drainArray = cs.toArray
						def apply = Vector.empty
						def applyAll = Vector(xs: _*)
						def foldL = cs.foldLeft(0)(_.hashCode + _.hashCode)
						def size = cs.size
						val _fixture  = fixture
						val _collapse = _.toSeq
					}
				case ScalaStream => val cs = xs.toStream
					new ImmutableOps {
						type A = X
						type C[x] = Stream[x]
						def head = cs.headOption
						def tail = cs.tail
						def append = cs :+ x
						def prepend = x +: cs
						def concat = cs ++ cs
						def drainArray = cs.toArray
						def apply = Stream.empty
						def applyAll = Stream(xs: _*)
						def foldL = cs.foldLeft(0)(_.hashCode + _.hashCode)
						def size = cs.size
						val _fixture  = fixture
						val _collapse = _.toSeq
					}

				case JavaArray       => val cs = xs.toArray
					new ImmutableOps {
						type A = X
						type C[x] = Array[x]
						def head = cs.headOption
						def tail = cs.tail
						def append = cs :+ x
						def prepend = x +: cs
						def concat = cs ++ cs
						def drainArray = cs.toArray
						def apply = Array.empty
						def applyAll = Array(xs: _*)
						def foldL = cs.foldLeft(0)(_.hashCode + _.hashCode)
						def size = cs.length
						val _fixture  = fixture
						val _collapse = _.toSeq
					}
				case CatsChainList   => mkChainOps(xs.toList)
				case CatsChainVector => mkChainOps(xs.toVector)
				case CatsChainStream => mkChainOps(xs.toStream)
				case CatsChainArray  => mkChainOps(xs.toArray[X])
				case bad             => sys.error(s"bad collection type `$bad`")
			}
		}

		@Setup(Level.Iteration) def setup(): Unit = ops = prepare(size, elementTpe)

	}


}