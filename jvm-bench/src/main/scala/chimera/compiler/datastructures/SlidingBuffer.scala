package chimera.compiler.datastructures

import chimera.compiler.datastructures.SlidingBuffer.Node

import scala.collection.generic._
import scala.collection.{AbstractIterator, mutable}

@SerialVersionUID(3419063961353022662L)
final class SlidingBuffer[A]
	extends mutable.AbstractBuffer[A]
		with mutable.Buffer[A]
		with mutable.IndexedSeq[A]
		with GenericTraversableTemplate[A, SlidingBuffer]
		with mutable.BufferLike[A, SlidingBuffer[A]]
		with Serializable {
	override def companion: GenericCompanion[SlidingBuffer] = SlidingBuffer

	// The design of this class is similar to a ListBuffer, but provides an additional scratch-pointer
	private var start  : Node[A] = _
	private var end    : Node[A] = start
	private var scratch: Node[A] = start
	private var pos    : Int     = 0
	private var len              = 0

	override def length: Int = len
	override def size: Int = len

	override def isEmpty: Boolean = len == 0
	override def nonEmpty: Boolean = len != 0

	override def apply(n: Int): A = navigate(n).x
	def update(n: Int, x: A): Unit = navigate(n).x = x

	def +=(x: A): this.type = {
		if (isEmpty) {
			start = new Node(x, null, null)
			end = start
			scratch = start
			pos = 0
		}
		else {
			val end_ = new Node(x, null, end)
			end.next = end_
			end = end_
		}
		len += 1
		this
	}

	def +=:(x: A): this.type = {
		if (isEmpty) {
			start = new Node(x, null, null)
			end = start
			scratch = start
			pos = 0
		}
		else {
			val start_ = new Node(x, start, null)
			start.prev = start_
			start = start_
			pos += 1
		}
		len += 1
		this
	}

	override def ++=(xs: TraversableOnce[A]): this.type = xs match {
		case x: AnyRef if x eq this => this ++= this.take(len)
		case _                      => super.++=(xs)
	}

	override def ++=:(xs: TraversableOnce[A]): this.type = {
		if (xs.asInstanceOf[AnyRef] eq this) ++=:(this.take(len))
		else {
			var elems = xs.toList
			var addedLen = 0
			if (isEmpty) {
				start = new Node(elems.head, start, null)
				scratch = start
				pos = 0
				addedLen = 1
				elems = elems.tail
			}
			for (elem <- elems.reverseIterator) {
				val newNode = new Node(elem, start, null)
				start.prev = newNode
				start = newNode
				addedLen += 1
				pos += 1
			}
			len += addedLen
			this
		}
	}

	def clear(): Unit = {
		start = null
		end = null
		scratch = null
		len = 0
		pos = 0
	}

	override def insertAll(n: Int, elems: Traversable[A]): Unit = {
		if (n == 0) ++=:(elems)
		else if (n == len) ++=(elems)
		else {
			if (n < 0 || n > len) throw new IndexOutOfBoundsException(n.toString)
			var temp = navigate(n)
			for (elem <- elems.toList.reverseIterator) {
				val newNode = new Node(elem, temp, temp.prev)
				temp.prev.next = newNode
				temp.prev = newNode
				scratch = newNode
				temp = newNode
				len += 1
			}
			scratch = temp
		}
	}

	def insert(n: Int, x: A): Unit = {
		if (n == 0) +=:(x)
		else if (n == len) +=(x)
		else {
			if (n < 0 || n > len) throw new IndexOutOfBoundsException(n.toString)
			val node = navigate(n)
			val newNode = new Node(x, node, node.prev)
			node.prev.next = newNode
			node.prev = newNode
			scratch = newNode
			len += 1
		}
	}

	def removeMany(ns: Int*): Unit = for (n <- ns.sorted) remove(n)
	override def remove(n: Int): A = {
		if (n < 0 || n > len) throw new IndexOutOfBoundsException("at " + n.toString)
		if (n == 0) {
			val x = start.x
			if (len > 1) start.next.prev = null
			start = start.next
			len -= 1
			if (pos != 0) pos -= 1
			else scratch = start
			x
		}
		else if (n == len - 1) {
			val x = end.x
			if (len > 1) end.prev.next = null
			end = end.prev
			len -= 1
			if (pos == len) {
				scratch = end
				pos -= 1
			}
			x
		}
		else {
			val node = navigate(n)
			scratch = node.next
			node.prev.next = scratch
			scratch.prev = node.prev
			len -= 1
			node.x
		}
	}

	override def remove(n: Int, count: Int): Unit = {
		if (count == 0) return
		if (count == 1) remove(n)
		else {
			if (n < 0 || n > len - count) throw new IndexOutOfBoundsException("at " + n.toString + " deleting " + count.toString)
			if (n == 0) {
				for (_ <- 0 until count) start = start.next
				if (start != null) start.prev = null
				if (pos > len) {
					pos = 0
					scratch = start
				}
				else pos -= count
			}
			else {
				var temp = navigate(n)
				val prev = temp.prev
				for (_ <- 0 until count) temp = temp.next
				if (temp != null) temp.prev = prev
				else end = prev
				prev.next = temp
				pos = n
				scratch = temp
			}
			len -= count
		}
	}

	override def iterator: Iterator[A] = new AbstractIterator[A] {
		var cursor: Node[A] = start
		def hasNext: Boolean = (cursor ne null) && (cursor.prev ne end) // fix for windows :)
		def next(): A = {
			if (!hasNext) throw new NoSuchElementException("next on empty Iterator")
			else {
				val ans = cursor.x
				cursor = cursor.next
				ans
			}
		}
	}

	override def reverseIterator: Iterator[A] = new AbstractIterator[A] {
		var cursor: Node[A] = end
		def hasNext: Boolean = (cursor ne null) && (cursor.next ne start) // fix for windows :)
		def next(): A = {
			if (!hasNext) throw new NoSuchElementException("next on empty Iterator")
			else {
				val ans = cursor.x
				cursor = cursor.prev
				ans
			}
		}
	}

	private def navigate(i: Int): Node[A] = {
		val n = len - 1
		if (i == 0) start
		else if (i == n) end
		else if (pos == i) scratch
		// optimise for next and prev
		else if (pos + 1 == i) {
			scratch = scratch.next
			pos += 1
			scratch
		}
		else if (pos - 1 == i) {
			scratch = scratch.prev
			pos -= 1
			scratch
		}
		else if (pos < i) {
			if (n - i < i - pos) {
				if (i > n) throw new IndexOutOfBoundsException(s"SlidingBuffer: cannot access index $i in a buffer of size $size")
				pos = n
				scratch = end
				moveDown(i)
			}
			else moveUp(i)
		}
		else if (i < pos - i) {
			if (i < 0) throw new IndexOutOfBoundsException(s"SlidingBuffer: cannot access index $i")
			pos = 0
			scratch = start
			moveUp(i)
		}
		else moveDown(i)
	}

	private def moveUp(i: Int): Node[A] = {
		do {
			scratch = scratch.next
			pos += 1
		} while (pos != i)
		scratch
	}

	private def moveDown(i: Int): Node[A] = {
		do {
			scratch = scratch.prev
			pos -= 1
		} while (pos != i)
		scratch
	}

	// Way more optimised version of view(from, until), slice speeds of access with very fast construction speeds
	def window(from: Int, until: Int): SlidingBuffer[A] = new SlidingBuffer[A](navigate(from), navigate(until - 1), until - from)

	protected def this(start: Node[A], end: Node[A], len: Int) = {
		this()
		this.start = start
		this.end = end
		this.len = len
		this.scratch = start
	}

	override def toString(): String = mkString("SlidingBuffer(", ", ", ")")
}

object SlidingBuffer extends IndexedSeqFactory[SlidingBuffer] {
	implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SlidingBuffer[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
	def newBuilder[A]: mutable.Builder[A, SlidingBuffer[A]] = new mutable.GrowingBuilder[A, SlidingBuffer[A]](new SlidingBuffer[A])
	class Node[A](var x: A, var next: Node[A], var prev: Node[A])
}
