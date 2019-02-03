package net.kurobako.col

import java.util
import java.util.concurrent.TimeUnit

import cats.data.Chain
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.util.Random


object CollectionBench {

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
	}

	@BenchmarkMode(Array(Mode.AverageTime))
	@OutputTimeUnit(TimeUnit.NANOSECONDS)
	@State(Scope.Benchmark)
	class Mutable {
		@Benchmark def append(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.append)
		@Benchmark def prepend(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.prepend)
		@Benchmark def concat(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.concat)
		@Benchmark def drainArray(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.drainArray)
		@Benchmark def index(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.index)
		@Benchmark def apply(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.apply)
		@Benchmark def applyAll(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.applyAll)
		@Benchmark def size(bh: Blackhole, input: MutableInput): Unit = bh.consume(input.ops.size)
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
		def size: Int
	}

	@BenchmarkMode(Array(Mode.AverageTime))
	@OutputTimeUnit(TimeUnit.NANOSECONDS)
	@State(Scope.Benchmark)
	class Immutable {
		@Benchmark def head(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.head)
		@Benchmark def tail(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.tail)
		@Benchmark def append(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.append)
		@Benchmark def prepend(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.prepend)
		@Benchmark def concat(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.concat)
		@Benchmark def drainArray(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.drainArray)
		@Benchmark def apply(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.apply)
		@Benchmark def applyAll(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.applyAll)
		@Benchmark def size(bh: Blackhole, input: ImmutableInput): Unit = bh.consume(input.ops.size)
	}

	private final val StringTpe = "String"
	private final val IntTpe    = "Int"

	private final val JavaArrayList    = "java.util.ArrayList"
	private final val JavaLinkedList   = "java.util.LinkedList"
	private final val ScalaListBuffer  = "mutable.ListBuffer"
	private final val ScalaArrayBuffer = "mutable.ArrayBuffer"

	private final val ScalaList   = "List"
	private final val ScalaVector = "Vector"
	private final val ScalaStream = "Stream"
	private final val CatsChain   = "Chain"
	private final val JavaArray   = "Array"

	type MkOp[A, B] = (Seq[A], A) => B
	private def prepare[A, B](size: Int, tpe: String)(implicit
													  strMkOp: MkOp[String, B],
													  intMkOp: MkOp[Int, B]): B = {
		val mid = size / 2
		val data = 0 to size
		tpe match {
			case StringTpe =>
				val xs = data.map(_.toString).toVector
				strMkOp(Random.shuffle(xs), xs(mid))
			case IntTpe    =>
				val xs = data.toVector
				intMkOp(Random.shuffle(xs), xs(mid))
		}
	}

	@State(Scope.Thread) class MutableInput {

		@Param(Array(
			//			"1",
			//			"10",
			//			"100",
			"1000",
			"10000",
			"100000",
			//			"1000000",
			//			"10000000",
		))
		var size: Int    = _

		@Param(Array(StringTpe, IntTpe))
		var cls : String = _

		@Param(Array(JavaArrayList, JavaLinkedList, ScalaListBuffer, ScalaArrayBuffer))
		var tpe: String = _

		var ops: MutableOps = _

		implicit def prepareMutable[X: ClassTag](xs: Seq[X], x: X): MutableOps = {
			import scala.collection.JavaConverters._
			val mid = xs.length / 2
			tpe match {
				case ScalaArrayBuffer => val cs = ArrayBuffer(xs: _*)
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
					}
				case ScalaListBuffer  => val cs = ListBuffer(xs: _*)
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
					}
				case JavaArrayList    =>
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
					}
				case JavaLinkedList   =>
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
					}
			}
		}


		@Setup(Level.Iteration) def setup(): Unit = ops = prepare(size, cls)

	}


	@State(Scope.Thread) class ImmutableInput {

		@Param(Array(
			//			"1",
			//			"10",
			//			"100",
			"1000",
			"10000",
			"100000",
			//			"1000000",
			//			"10000000",
		))
		var size: Int = _

		@Param(Array(StringTpe, IntTpe))
		var cls: String = _

		@Param(Array(ScalaList, ScalaVector, ScalaStream, CatsChain, JavaArray))
		var tpe: String = _

		var ops: ImmutableOps = _

		private implicit def prepareImmutable[X: ClassTag](xs: Seq[X], x: X): ImmutableOps = tpe match {
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
					def size = cs.size
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
					def size = cs.size
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
					def size = cs.size
				}
			case CatsChain   => val cs = Chain.fromSeq(xs)
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
					def size = cs.length.toInt
				}
			case JavaArray   => val cs = xs.toArray
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
					def size = cs.length
				}
		}

		@Setup(Level.Iteration) def setup(): Unit = ops = prepare(size, cls)

	}


}