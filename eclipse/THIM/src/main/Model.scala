package main

import scala.util.Random
import scala.collection.mutable.ArraySeq
import java.util.Scanner
import Util._
import scala.math
import scala.language.postfixOps
import scala.language.implicitConversions

class Model(val startingSims: Int, val numNbhd: Int) {
    import Model.Age, Model.Income, Model.Education, Model.Health

    var nbhds = Vector.fill(numNbhd)(new Neighborhood())
    var sims = Vector.fill(startingSims)(new Sim(None, Model.EB0, randomElement(nbhds)))

    def nextState() {
        /*
         * COMPUTE STATISTICS
         */
        val avgNbhdIncomes =
            sims groupBy (_.nbhd) mapValues (_ map (_.income) average)

        val avgIncome = sims.map(_.income).average
        val avgHealth = sims map (_.health) average // n.b. periods between object and methods are optional

        def avgIncomeNearAge(age: Age): Income = {
            def isInAgeGroup(sim: Sim) = Math.abs(age - sim.age) < Model.AGE_GROUP_SIZE

            return sims filter (isInAgeGroup) map (_.income) average
        }

        /*
         * PERFORM AGENT-INTERNAL UPDATES
         */
        // age everyone by a year
        sims.foreach(_.incrAge())

        // generate incomes for newly educated sims
        sims filter (_.justDoneEducation) foreach (sim =>
            sim.baseIncome = Model.startingIncomeBase(avgIncome,
                                                      avgNbhdIncomes(sim.nbhd),
                                                      sim))

        // update real income
        sims filter (_.doneEducation) foreach (sim =>
            sim.income = Model.computeAnnualIncome(sim))

        // update health 
        sims.foreach(sim =>
            sim.health = Model.computeHealth(avgIncomeNearAge(sim.age), sim))

        /*
         *  MORTALITY
         */
        val (deadsims, survivors) = sims partition (sim =>
            randTrue(Model.deathProbability(avgHealth, avgIncome, sim)))

        // remove dead parents of children    
        survivors filter (sim => deadsims.contains(sim.parent)) foreach (sim =>
            sim.parent = None)

        /*
         * BIRTH
         */
        // helper function - generates child for a given parent
        def genChild(parent: Sim) =
            Sim.generateChild(parent,
                              Model.computeEducation(parent.income,
                                                     avgNbhdIncomes(parent.nbhd),
                                                     avgIncome))

        val newborns = survivors filter
            Model.fertile filter
            (_ => randTrue(Model.BIRTH_PROBABILITY)) map
            genChild

        sims = survivors ++ newborns

        /*
         * MOBILITY
         */
        val attemptedMoves = sims filter
            (_.doneEducation) filter
            (sim => randTrue(Model.moveEffort(avgNbhdIncomes(sim.nbhd), sim)))

        def incomeDifference(sim: Sim)(pair: (Neighborhood, Income)) =
            math.abs(sim.income - pair._2)

        def bestNbhd(sim: Sim) = avgNbhdIncomes.minBy(incomeDifference(sim))._1

        // for now, assume all move attempts succeeded (ignore capacities)
        for (sim <- attemptedMoves) {
            sim.nbhd = bestNbhd(sim);
            sim.livesAtHome = false;
        }

        // move dependent children along with their parents
        sims filter (_.livesAtHome) filter (_.parent.nonEmpty) foreach (child =>
            child.nbhd = child.parent.get.nbhd)
    }
}

/**
 * Stores parameters and formulas
 */
object Model extends App {

    type Education = Int
    type Income = Double
    type Age = Int
    type Health = Double

    // EDUCATION 

    val EB0: Education = 20 // mean education
    val EB1: Double = 0 // influence of relative neighborhood income on education
    val EB2: Double = 0 // influence of relative parent income on education
    val Esigma: Double = 0 // standard deviation of education

    def computeEducation(parentalIncome: Income, avgNbhdIncome: Income, avgGlobalIncome: Income) =
        (EB0 +
         EB1 * math.log(avgNbhdIncome / avgGlobalIncome) +
         EB2 * math.log(parentalIncome / avgGlobalIncome) +
         Distribution.normal(0, Esigma).draw()) toInt

    // INCOME  

    val YB1: Double = 0.1 // influence of relative educational attainment
    val YB2: Double = 0.1 // influence of relative parental income
    val YB3: Double = 0.1 // influence of relative neighborhood income
    val Ysigma: Double = 0.1
    val YInit: Distribution[Double] = Distribution.normalPos(1, 1) // base distribution of income
    val ageIncomeProfile: Age => Income =
        (a) => 1.0 // TODO add actual income profile 

    def startingIncomeBase(avgGlobalIncome: Income, avgNbhdIncome: Income, sim: Sim): Double =
        YInit.draw() +
        YB1 * math.log(sim.education.toDouble / EB0) +
        YB2 * math.log(sim.parent.map(_.income).getOrElse(avgGlobalIncome) / avgGlobalIncome) +
        YB3 * math.log(avgNbhdIncome / avgGlobalIncome)

    def computeAnnualIncome(sim: Sim) =
        sim.baseIncome * ageIncomeProfile(sim.age) * math.exp(Distribution.normal(0, Ysigma).draw())

    // HEALTH

    val MAX_HEALTH = 1.0
    val MIN_HEALTH = 0.0
    val HB1: Double = 0 // influence of relative wealth on health
    val AGE_GROUP_SIZE = 10 // size of age group to use for "average income near age" calculation

    val healthRandomWalk = Distribution.normal(-0.01, 0.1) // TODO make actual random walk

    def computeHealth(averageIncomeNearAge: Income, sim: Sim): Health =
        truncate(0,
                 1,
                 sim.health +
                     HB1 * math.log(sim.income / averageIncomeNearAge) +
                     healthRandomWalk.draw())

    // BIRTH 

    val FERTILITY_RANGE: (Age, Age) = (20, 40)
    val BIRTH_PROBABILITY = 0.1

    def fertile(sim: Sim) = sim.age >= FERTILITY_RANGE._1 && sim.age <= FERTILITY_RANGE._2

    // MORTALITY  

    val MB1 = 0
    val MB2 = 0
    val ageMortalityRate: Age => Double =
        (a) => 0.01 // TODO add actual mortality schedule (given as piecewise linear function of age)

    def deathProbability(avgGlobalHealth: Health, avgGlobalIncome: Income, sim: Sim) =
        ageMortalityRate(sim.age) *
            math.pow(avgGlobalHealth / sim.health, MB1) *
            math.pow(avgGlobalIncome / sim.income, MB2)

    // SPATIAL MOBILITY

    val Ymin: Double = 0.2 // minimum relative income difference at which sim tries to move away
    val Ymax: Double = 0.5 // minimum relative income difference at which sim tries to move away with 100 % effort

    def moveEffort(avgNbhdIncome: Income, sim: Sim): Double = {
        val fractionAway = math.abs(sim.income - avgNbhdIncome) / avgNbhdIncome
        if (fractionAway <= Ymin) 0
        else if (fractionAway >= Ymax) 1
        else (fractionAway - Ymin) / (Ymax - Ymin)
    }

    // runs model for 100 iterations with user-specified number of sims and iterations
    override def main(args: Array[String]) {
        val numSims = Integer.parseInt(args(1))
        val numIterations = Integer.parseInt(args(0))

        val model = new Model(numSims, 50)

        for (x <- 0 to numIterations)
            model.nextState()
    }
}