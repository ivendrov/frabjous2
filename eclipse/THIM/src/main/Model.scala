package src.main

import scala.util.Random
import scala.collection.mutable.ArraySeq
import java.util.Scanner
import Util._
import scala.math
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.mutable.HashSet

class Model(val startingSims: Int, val numNbhd: Int, val nbhdCapacity : Int) {
    import Model.Age, Model.Income, Model.Education, Model.Health

    
    val eventQueue : EventQueue[ModelEvent] = new EventQueue() // TODO init properly
    
    // global data structures
    val nbhds = Vector.fill(numNbhd)(new Neighborhood())
    val sims = HashSet(Seq.fill(startingSims)(new Sim(None, Model.EB0, randomElement(nbhds))) : _*) // TODO init properly
    
    // global statistics
    var avgIncome : Income = 0 
    var avgHealth : Health = 0 
    var avgNbhdIncomes : Map[Neighborhood, Income] = Map.empty
    def avgIncomeNearAge(age: Age): Income = { // TODO make more efficient
        def isInAgeGroup(sim: Sim) = Math.abs(age - sim.age) < Model.AGE_GROUP_SIZE
        return sims filter (isInAgeGroup) map (_.income) average
    }
    
    /**
     * processes the next event in the event queue; TODO return information for statistic collection?
     */
    def processNextEvent(){
        eventQueue.nextEvent() match {
            case Birth(parent) => {
                val child = Sim.generateChild(parent,
                              	  			  Model.computeEducation(parent.income,
                              	  					  				 parent.nbhd.avgIncome,
                              	  					  				 avgIncome))
                sims.add(child)
                eventQueue.addEvent(Birthday(child), 1.0);
            }
            case Birthday(sim) => {
                // 1. increment age
                sim.incrAge()
                
                // 2. generate income if newly educated
                if (sim.justDoneEducation) 
                    sim.baseIncome = Model.startingIncomeBase(avgIncome,
                                                      		  sim.nbhd.avgIncome,
                                                      		  sim)
                                                      		  
                // 3. update real income
                if (sim.doneEducation)
                    sim.income = Model.computeAnnualIncome(sim)
                
                // 4. update health
                sim.health = Model.computeHealth(avgIncomeNearAge(sim.age), sim)
                
                // 5. mortality calculation
                if (randTrue(Model.deathProbability(avgHealth, avgIncome, sim)))
                    eventQueue.addEvent(Death(sim), 0.0)
                else {                    
	                // 6. birth calculation
	                if (Model.fertile(sim) && randTrue(Model.BIRTH_PROBABILITY))
	                    eventQueue.addEvent(Birth(sim), Distribution.uniform(0,1).draw())
	                    
	                // 7. movement calculation
	                if (sim.doneEducation && randTrue(Model.moveEffort(sim.nbhd.avgIncome, sim))){
	                    eventQueue.addEvent(Movement(sim), 0.0)
	                }
                }

            }
            case Death(sim) => {
                sims.remove(sim)
                // TODO remove yourself from children, parents, network etc
            }
            case Movement(sim) => {
                val bestNbhd = nbhds.minBy(nbhd => math.abs(nbhd.avgIncome - sim.income))
                if (bestNbhd.hasSpace){
                    // perform movement
                    sim.nbhd = bestNbhd
                    sim.livesAtHome = false
                    // TODO move dependent children (how? need to keep track of children, or else 
                    // don't let dependents actually have their own neighborhood
                }
            }
            case ComputeStatistics() => {
            	avgNbhdIncomes =
            			sims groupBy (_.nbhd) mapValues (_ map (_.income) average)
            	avgIncome = sims.map(_.income).average
            	avgHealth = sims map (_.health) average // n.b. periods between object and methods are optional
            }
        }
        
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
}