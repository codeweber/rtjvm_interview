package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T]: 
    def head: T
    def tail: RList[T]
    def isEmpty: Boolean

    def :+:[S >: T](elem: S): RList[S] = new :+:(elem, this)

    @tailrec
    final def apply(index: Int): T = 
        if index < 0 then
            throw new IllegalArgumentException("index should be positive")
        else
            this match
                case RNil => throw new NoSuchElementException
                case h :+: t => 
                    if index == 0 then 
                        h
                    else
                        t(index - 1)

    def length: Int 

    def reverse: RList[T]

    // concatenate a list
    def ++[S >: T](anotherList: RList[S]): RList[S]

    // remove at kth element
    def removeAt(index: Int): RList[T]

    def map[S](f: T => S): RList[S]
    def flatMap[S](f: T => RList[S]): RList[S]
    def filter(predicate: T => Boolean): RList[T]

    def rle: RList[(T,Int)]

    def duplicateEach(k: Int): RList[T]

    def rotate(shift: Int): RList[T]

    def sample(k: Int): RList[T]

    def sorted[S >: T](using ordering: Ordering[S]): RList[S]

    def mergeSorted[S >: T](using ordering: Ordering[S]): RList[S]

    def quickSorted[S >: T](using ordering: Ordering[S]): RList[S]



case object RNil extends RList[Nothing]:
    override def head: Nothing = throw new NoSuchElementException
    override def tail: RList[Nothing] = throw new NoSuchElementException 
    override def isEmpty: Boolean = true 

    override def toString: String = "[]"

    override val length = 0

    override def reverse: RList[Nothing] = RNil

    override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

    override def removeAt(k: Int): RList[Nothing] = RNil

    override def map[S](f: Nothing => S): RList[S] = RNil
    override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
    override def filter(predicate: Nothing => Boolean): RList[Nothing] = RNil 

    override def rle: RList[(Nothing, Int)] = RNil

    override def duplicateEach(k: Int): RList[Nothing] = RNil

    override def rotate(shift: Int): RList[Nothing] = RNil

    override def sample(k: Int): RList[Nothing] = RNil

    override def sorted[S >: Nothing](using ordering: Ordering[S]): RList[S] = RNil

    override def mergeSorted[S >: Nothing](using ordering: Ordering[S]): RList[S] = RNil

    override def quickSorted[S >: Nothing](using ordering: Ordering[S]): RList[S] = RNil


case class :+:[T](override val head: T, override val tail: RList[T]) extends RList[T]:
    override def isEmpty: Boolean = false

    override def toString: String =

        @tailrec
        def toStringTailRec(remaining: RList[T], result: String): String =
            remaining match
                case RNil => s"$result :: ${RNil.toString}"
                case :+:(h, t) => toStringTailRec(t, s"$result :: $h")


        toStringTailRec(tail, s"$head")

    override val length = 1 + tail.length

    override def reverse: RList[T] =

        @tailrec 
        def loop(remaining: RList[T], done: RList[T]): RList[T] = 
            if (remaining.isEmpty) then
                done
            else
                loop(remaining.tail, remaining.head :+: done)
        
        loop(this, RNil)

    
    override def ++[S >: T](anotherList: RList[S]): RList[S] = 

/*         @tailrec 
        def loop(remaining: RList[T], agg: RList[S]): RList[S] = 
            if remaining.isEmpty then
                agg 
            else 
                loop(remaining.tail, remaining.head :+: agg) */

        RList.reverseConcat(this.reverse, anotherList)

    override def removeAt(k: Int): RList[T] = 

        @tailrec
        def loop(idxAtRemainingHead: Int, popped: RList[T], remaining: RList[T]): RList[T] = 
            if remaining.isEmpty then
                    popped.reverse 
            else
                if idxAtRemainingHead == k then
                    RList.reverseConcat(popped, remaining.tail)
                else 
                    loop(idxAtRemainingHead + 1, remaining.head :+: popped, remaining.tail)


        if k < 0 then 
            this 
        else if k == 0 then 
            this.tail 
        else
            loop(1, this.head :+: RNil , this.tail)


    def map[S](f: T => S): RList[S] = 

        @tailrec 
        def loop(remaining: RList[T], processed: RList[S]): RList[S] = 
            remaining match
                case RNil => processed.reverse
                case x :+: xs => loop(xs, f(x) :+: processed)

        loop(this, RList.empty[S])


    def flatMap[S](f: T => RList[S]): RList[S] = 

        @tailrec 
        def loop(remaining: RList[T], processed: RList[S]): RList[S] = 
            remaining match
                case RNil => processed.reverse
                case x :+: xs => loop(xs, RList.reverseConcat(f(x),processed))

        loop(this, RList.empty[S])

    def filter(predicate: T => Boolean): RList[T] =

        @tailrec 
        def loop(remaining: RList[T], processed: RList[T]): RList[T] = 
            remaining match
                case RNil => processed.reverse 
                case x :+: xs => 
                    loop(xs, if predicate(x) then x :+: processed else processed)
                    
        loop(this, RList.empty[T])

        //note that filter could be implemented in terms of flatMap
        //this.flatMap(x => if predicate(x) then x :+: RNil else RNil)


    override def rle: RList[(T, Int)] =

        @tailrec 
        def loop(remaining: RList[T], agg: RList[(T, Int)]): RList[(T,Int)] =

            if remaining.isEmpty then 
                agg.reverse 
            else
                agg match
                    case (h, n) :+: t if h == remaining.head => loop(remaining.tail, (h, n+1) :+: t)
                    case _ => loop(remaining.tail, (remaining.head, 1) :+: agg)

        loop(this, RList.empty[(T,Int)])

    def duplicateEach(k: Int): RList[T] =

        @tailrec
        def loop(element: T, copiesRemaining: Int, remaining: RList[T], agg: RList[T]): RList[T] = 
            if copiesRemaining == 0 then
                if remaining.isEmpty then
                    agg 
                else 
                    loop(remaining.head, k, remaining.tail, agg)
            else 
                loop(element, copiesRemaining-1, remaining, element :+: agg )

        if k < 0 then 
            RNil 
        else if k == 1 then 
            this 
        else
            val r = this.reverse
            loop(r.head, k-1, r.tail, r.head :+: RList.empty[T])


    override def rotate(shift: Int): RList[T] = 

        @tailrec 
        def loop(remaining: RList[T], shiftRemaining: Int, poppedElements: RList[T]): RList[T] = 
            if shiftRemaining == 0 then 
                remaining ++ poppedElements.reverse 
            else
                loop(remaining.tail, shiftRemaining - 1, remaining.head :+: poppedElements)

        val len = this.length
        val shiftMod = shift % len
        if shiftMod == 0 || len == 1 then 
            this
        else if shiftMod < 0 then
            loop(this.tail, (len+shiftMod)-1, this.head :+: RNil)
        else 
            loop(this.tail, shiftMod-1, this.head :+: RNil)

    override def sample(k: Int): RList[T] = 

        import scala.util.Random
        val r = Random(System.currentTimeMillis)
        val len = this.length

        RList.from(1 to k).map(_ => this(r.nextInt(len)))

    def sorted[S >: T](using ordering: Ordering[S]): RList[S] = 

        @tailrec
        def insertElement(x: S, searchedItems: RList[S], remainingItems: RList[S]): RList[S] = 

            remainingItems match 
                case RNil => (x :+: searchedItems).reverse 
                case y :+: ys if ordering.compare(x, y) > 0 => insertElement(x, y :+: searchedItems, ys)
                case y :+: ys => RList.reverseConcat(x :+: searchedItems, remainingItems)


        @tailrec
        def loop(remaining: RList[S], orderedAgg: RList[S]): RList[S] =

            remaining match
                case RNil => orderedAgg 
                case x :+: xs => loop(xs, insertElement(x, RNil, orderedAgg))

        loop(this, RNil)

    def mergeSorted[S >: T](using ordering: Ordering[S]): RList[S] = 

        @tailrec
        def mergeLists(l1: RList[S], l2: RList[S], agg: RList[S]): RList[S] = 

            l1 match
                case RNil => RList.reverseConcat(agg, l2)
                case x :+: xs =>
                    l2 match 
                        case RNil => RList.reverseConcat(agg, l1)
                        case y :+: ys => 
                            if ordering.lteq(x, y) then
                                mergeLists(xs, l2, x :+: agg)
                            else 
                                mergeLists(l1, ys, y :+: agg)

        /* 
        def splitAt(l: RList[S], index: Int): (RList[S], RList[S]) = 

            @tailrec
            def splitTailRec(depth: Int, first: RList[S], second: RList[S]): (RList[S], RList[S]) =

                if depth >= index then
                    (first, second)
                else
                    splitTailRec(depth + 1, first.tail, first.head :+: second)

            splitTailRec(0, this, RNil)

        val midPoint = this.length / 2
        if midPoint < 1 then
            this
        else 
            val (l1, l2) = splitAt(this, midPoint)
            println(s"List 1: $l1, List 2: $l2")
            mergeLists(l1.mergeSorted, l2.mergeSorted, RNil)    
        */

        @tailrec
        def loop(smallLists: RList[RList[S]], bigLists: RList[RList[S]]): RList[S] = 
            if smallLists.isEmpty then
                if bigLists.isEmpty then
                    RNil 
                else 
                    if bigLists.tail.isEmpty then 
                        bigLists.head 
                    else
                        loop(bigLists, RNil)
            else
                if smallLists.tail.isEmpty then 
                    loop(RNil, smallLists.head :+: bigLists)
                else 
                    loop(smallLists.tail.tail, mergeLists(smallLists.head, smallLists.tail.head, RNil) :+: bigLists)

        loop(this.map(x => x :+: RNil), RList.empty[RList[S]])

    override def quickSorted[S >: T](using ordering: Ordering[S]): RList[S] =
/* 
        loop([5,4,2,1], [], [])
        loop([2,1], [[5,4]], [])
        loop([1], [[2], [5,4]], [])
        loop([], [[2], [5,4]], [1])
        loop([2], [[5,4]], [1])
        loop([], [[5,4]], [2,1])
        loop([5,4],[], [2,1])
        loop([4], [[5]], [2,1])
        loop([], [[5]], [4,2,1])
        loop([5], [], [4,2,1])
        loop([], [], [5,4,2,1])
 */
        @tailrec
        def splitByPivot(l: RList[S], pivot: S, lessThan: RList[S], equalTo: RList[S], greaterThan: RList[S]): (RList[S], RList[S], RList[S]) = 

            l match 
                case RNil => (lessThan, equalTo, greaterThan)
                case x :+: xs if ordering.lt(x, pivot) => splitByPivot(xs, pivot, x :+: lessThan, equalTo, greaterThan)
                case x :+: xs if ordering.gt(x, pivot) => splitByPivot(xs, pivot, lessThan, equalTo, x :+: greaterThan)
                case x :+: xs => splitByPivot(xs, pivot, lessThan, x :+: equalTo, greaterThan)

        @tailrec
        def loop(lessThan: RList[S], biggerThanStack: RList[RList[S]], agg: RList[S]): RList[S] = 

            if lessThan.isEmpty then
                if biggerThanStack.isEmpty then
                    agg.reverse
                else 
                    loop(biggerThanStack.head, biggerThanStack.tail, agg)
            else 

                val midPoint = lessThan.length/2
                if midPoint < 1 then 
                    if biggerThanStack.isEmpty then 
                        (lessThan.head :+: agg).reverse
                    else
                        loop(biggerThanStack.head, biggerThanStack.tail, lessThan.head :+: agg)
                else 
                    val p = lessThan(midPoint)
                    val (lt, eq, gt) = splitByPivot(lessThan, p, RNil, RNil, RNil)
                    if lt.isEmpty then
                        loop(gt, biggerThanStack, RList.reverseConcat(eq, agg))
                    else
                        if gt.isEmpty then
                            loop(lt, eq :+: biggerThanStack, agg)
                        else 
                            loop(lt, eq :+: gt :+: biggerThanStack, agg)

        loop(this, RNil, RNil)


end :+:

object RList:

    def empty[T]: RList[T] = RNil

    def from[T](iterable: Iterable[T]): RList[T] =

        @tailrec 
        def loop(it: Iterable[T], coll: RList[T]): RList[T] = 
            if it.isEmpty then 
                coll
            else loop(it.tail, it.head :+: coll)

        loop(iterable, RNil).reverse

    @tailrec 
    private[lists] def reverseConcat[T, S >: T](listToReverse: RList[T], otherList: RList[S]): RList[S] = 
        if listToReverse.isEmpty then
                otherList 
        else 
            reverseConcat(listToReverse.tail, listToReverse.head :+: otherList)

    @main def TryListProblems = 

        val startTime = System.currentTimeMillis()
        RList.from(1 to 10000).flatMap(x => x :+: 2*x :+: RNil )
        println(System.currentTimeMillis() - startTime)