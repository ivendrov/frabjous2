package main

import scala.util.Random
import scala.collection.mutable.ArraySeq
import java.util.Scanner
import Util._
import scala.math
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.immutable.ParVector

class Model(val startingSims : Int, val numNbhd : Int){
  /*
   * Utility Methods
   */
  def randomElement[A](seq : IndexedSeq[A]): A = {
    val index = Random.nextInt(seq.length)
    seq(index)
  }
  
  var nbhds = IndexedSeq.fill(numNbhd)(new Neighborhood())  
  var sims = ParVector.fill(startingSims)(new Sim(None, 20, randomElement(nbhds)))
  sims.foreach(sim => sim.income = 10)
  
  println(nbhds(0).eq(nbhds(1)))

  def printState(){
    println("------------------------------")
    println("MODEL STATE:")
    val simsByNbhd = sims.groupBy(_.nbhd)
    for (nbhd <- nbhds){
      println(nbhd)
      if (simsByNbhd.contains(nbhd))
    	  simsByNbhd(nbhd).foreach(a => println("   " + a))
    }    
  }
  
  
  			
  			
  
  def nextState(){
     Util.startLog()
    
     /*
      * COMPUTE STATISTICS
      */
     val avgNbhdIncomes  = 
      sims groupBy (_.nbhd) mapValues (sims => sims.map(_.income).average)
     
     val avgIncome = avgNbhdIncomes.values.average /* or the more obviously correct
      													  * sims.map(_.income).average
                     									  */
     val avgHealth = sims.map(_.health).average
     
     def avgIncomeNearAge(age : Age) = // TODO make more efficient
       sims filter (sim => Math.abs(age - sim.age) < Model.AGE_GROUP_SIZE) map
     		(_.income) average
     		
    Util.logTime("done statistics")
    
    def transition (sim : Sim) : Sim = 
	    new Sim(sim.parent,
	    		sim.education,
	    		sim.nbhd,
	    		sim.age + 1,
	    		(if (sim.justDoneEducation) Model.startingIncomeBase(avgIncome,
	    		   									 avgNbhdIncomes(sim.nbhd),
	    		   									 sim)
	  			else sim.baseIncome),
	  			Model.computeAnnualIncome(sim),
	  			Model.computeHealth(avgIncomeNearAge(sim.age), sim),
	  			sim.livesAtHome)
     
     sims = sims.map(transition)
     /*
     
     /*
      * PERFORM AGENT-INTERNAL UPDATES
      */
     // age everyone by a year
     sims.foreach(_.incrAge())
     
     Util.logTime("incr age")
     
     // generate incomes for newly educated sims
     sims filter(_.justDoneEducation) foreach(sim => 
       sim.baseIncome = Model.startingIncomeBase(avgIncome,
    		   									 avgNbhdIncomes(sim.nbhd),
    		   									 sim))
     
    Util.logTime("incomeBase")		   									 
     
     // update real income
     sims filter(_.doneEducation) foreach (sim => 
       sim.income = Model.computeAnnualIncome(sim))
       
     Util.logTime("income")	
     // update health 
     sims.foreach(sim => 
       sim.health = Model.computeHealth(avgIncomeNearAge(sim.age), sim))
       */
       
     Util.logTime("health")	
     /* TEMP
      
     /*
      * DEATH
      */
     
     val (deadsims, livesims) = sims.partition(sim => 
       randTrue(Model.deathProbability(avgHealth, avgIncome, sim)))
       
     
     // remove dead parents of children    
     livesims.filter(sim => deadsims.contains(sim.parent)).foreach(sim =>
       sim.parent = None)
       
     /*
      * BIRTH
      */
     val newsims = livesims filter 
     				Model.fertile filter 
     				(_ => randTrue(Model.BIRTH_PROBABILITY)) map 
     				(sim => Sim.generateChild(sim, avgNbhdIncomes(sim.nbhd)))
     				
     sims = livesims ++ newsims
     				
     
     /*
      * MOBILITY
      */
     val attemptedMoves = sims filter 
     						(_.doneEducation) filter
     						(sim => randTrue(Model.moveEffort(avgNbhdIncomes(sim.nbhd),
     														  sim)))
     // for now, assume all move attempts succeed
     def bestNbhd (sim : Sim) = avgNbhdIncomes.minBy( t => math.abs(t._2 - sim.income))._1
     
     for (sim <- attemptedMoves){
       val destination = bestNbhd(sim)
       // move sim
       sim.nbhd = destination ; sim.livesAtHome = false
       // move children of sim that live at home (TODO make less grotesquely inefficient)
       for (s <- sims if s.parent == Some(sim) && s.livesAtHome)
         s.nbhd = destination
               
  	 }    
  	 *    */
  	 
  }
}

object Model {  
  /*
   * MODEL-WIDE CONSTANTS AND FORMULAS
   */
  
  val EB0 : Education = 20 // mean education
  val EB1 : Double= 0 // influence of relative neighborhood income on education
  val EB2 : Double = 0 // influence of relative parent income on education
  val Esigma : Double = 0 // standard deviation of education
  
  
  val YB1 : Double = 0.1 // influence of relative educational attainment
  val YB2 : Double = 0.1 // influence of relative parental income
  val YB3 : Double = 0.1 // influence of relative neighborhood income
  val Ysigma : Double = 0.1 
  val YInit : Distribution[Double]= Distribution.normalPos(1, 1) // base distribution of income
  
  val AGE_GROUP_SIZE = 10
  
  val HB1 : Double = 0 // influence of relative wealth on health
  val healthRandomWalk = Distribution.normal(-0.01, 0.1) // TODO make actual random walk
  
  val MB1 = 1
  val MB2 = 1
  val ageMortalityRate : Age => Double =
    (a) => 0.01 // TODO add mortality schedule
  
  val ageIncomeProfile : Age => Income = 
    (a) => 1.0 // TODO add income schedule
    
  val FERTILITY_RANGE : (Age, Age) = (20, 40)
  
  val BIRTH_PROBABILITY = 0.1
  
  // parameters for moveEffort (in percent)
  val Ymin : Double = 0.2
  val Ymax : Double = 0.5
  
  def moveEffort(avgNbhdIncome : Income, sim: Sim) : Double = {
      val fractionAway = math.abs(sim.income - avgNbhdIncome) / avgNbhdIncome //TODO add divzero check
      if (fractionAway <= Ymin) 0
      else if (fractionAway >= Ymax) 1
      else (fractionAway - Ymin) / (Ymax - Ymax)
  }
    
      
  
  
  
  def startingIncomeBase(avgGlobalIncome: Income,		  				 
  						avgNbhdIncome: Income,
  						sim : Sim) : Double =   
  	YInit.draw() +  	
    YB1 * math.log(sim.education.toDouble / EB0) + 
    YB2 * math.log(sim.parent.map(_.income).getOrElse(avgGlobalIncome) / avgGlobalIncome) +
    YB3 * math.log(avgNbhdIncome / avgGlobalIncome)
    
  def computeAnnualIncome(sim : Sim) = 
    sim.baseIncome * ageIncomeProfile(sim.age) * math.exp(Distribution.normal(0,Ysigma).draw())
  
  def computeHealth(averageIncomeNearAge: Income, sim : Sim) : Health = 
    truncate(0,
    		 1,
    		 sim.health + 
    		 HB1 * Math.log(sim.income / averageIncomeNearAge) + // TODO check for div zero
    		 healthRandomWalk.draw()) 
    		 
  def deathProbability(avgGlobalHealth: Health, avgGlobalIncome: Income, sim : Sim) =
    ageMortalityRate(sim.age) * 
    math.pow(avgGlobalHealth / sim.health, MB1) * 
    math.pow(avgGlobalIncome / sim.income, MB2)
    
  def fertile(sim : Sim) = sim.age >= FERTILITY_RANGE._1 && sim.age <= FERTILITY_RANGE._2
  
  
  	
  
  
  def main(args : Array[String]){
    val numAgents = Integer.parseInt(args(1))
    val numIterations = Integer.parseInt(args(0))
    
    
    val model = new Model(numAgents, 50)
    
    var x = 0
    
    val prevTime = System.nanoTime()
    
    
    while ( x < numIterations){
      model.nextState()
      x += 1
    }

    
    val newTime = System.nanoTime()
    println("Time for model run: " + (newTime - prevTime).toDouble / 1000000000)
    
    /*model.printState()
    
    val sc = new Scanner(System.in) 
    while (sc.nextLine != "q"){
      model.nextState()
      model.printState()
    }*/
    
    
  }
}