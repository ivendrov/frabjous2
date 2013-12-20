package src.main

import scala.util.Random

class Distribution[A](private val transform: Random => A) {
    private val gen: Random = new Random()
    def draw() = transform(gen)
}

object Distribution {
	
    private def tnormal(mean: Double, sdev: Double)(gen: Random) =
        Random.nextGaussian * sdev + mean

    def uniform(lo : Double, hi: Double): Distribution[Double] = {
        def tuniform(gen: Random) = 
            gen.nextDouble * (hi - lo) + lo
        new Distribution(tuniform)
    }
    
    def normal(mean: Double, sdev: Double): Distribution[Double] =
        new Distribution(tnormal(mean, sdev))

    private def tnormalPos(mean: Double, sdev: Double)(gen: Random) =
        Math.max(0, tnormal(mean, sdev)(gen))

    /**
     * normal curve with all negative values truncated
     */
    def normalPos(mean: Double, sdev: Double) =
        new Distribution(tnormalPos(mean, sdev))
}