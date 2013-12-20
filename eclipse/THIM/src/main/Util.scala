package src.main

import scala.collection.GenTraversableOnce
import scala.util.Random
import scala.language.implicitConversions

// defines a set of utility methods not specific to the THIM model
object Util {

    /**
     * selects a random element from the given sequence
     */
    def randomElement[A](seq: IndexedSeq[A]): A = {
        val index = Random.nextInt(seq.length)
        seq(index)
    }

    def truncate(min: Double, max: Double, value: Double) =
        Math.max(0,
            Math.min(1,
                value))

    /**
     *  returns true with probability "prob"
     */
    def randTrue(prob: Double) =
        Random.nextDouble() < prob

    /**
     * Logging helper functions - useful for measuring the amount of time spent on particular
     * portions of the program
     */
    var prevTime: Long = 0
    def startLog() {
        prevTime = System.nanoTime()
    }
    def logTime(note: String) {
        val next = System.nanoTime()
        println(note + (next - prevTime).toDouble / 1000000000)
        prevTime = next
    }

    /*
     * Defines an implicit "average" function - parallelize eventually
     */
    class EnrichedAvgFractional[A](self: GenTraversableOnce[A]) {
        def average(implicit num: Fractional[A]) = {
            val (total, count) = self.toIterator.foldLeft((num.zero, num.zero)) {
                case ((total, count), x) => (num.plus(total, x), num.plus(count, num.one))
            }
            num.div(total, count)
        }
    }
    implicit def enrichAvgFractional[A: Fractional](self: GenTraversableOnce[A]) =
        new EnrichedAvgFractional(self)

    class EnrichedDiv0Fractional[A](self: A) {
        // just like "div", but in case of division by zero, return 0
        def divOrZero(that: A)(implicit num: Fractional[A]): A =
            if (that == num.zero) num.zero
            else num.div(self, that)

    }
    implicit def enrichDiv0Fractional[A: Fractional](self: A) =
        new EnrichedDiv0Fractional(self)

}