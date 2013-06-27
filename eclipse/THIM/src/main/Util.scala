package main

import scala.collection.GenTraversableOnce
import scala.util.Random

object Util {
  type Income = Double
  type Age = Int
  type Education = Int
  type Health = Double
  
  
    
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
	
	
	def truncate(min : Double, max : Double, value: Double) =
	  Math.max(0,
			   Math.min(1,
			       value))
			       
	/*
	 *  returns true with probability "prob" 
	 */		       
	def randTrue(prob : Double) =
	  Random.nextDouble() < prob
}