package main

class Sim (var parent : Option[Sim], // could potentially not have a living parent		          
		   val education : Int, 
		   var nbhd : Neighborhood,
		   var age : Int = 0,
		   var baseIncome : Double = 0,
		   var income : Double = 0,
		   var health : Double = Sim.MAX_HEALTH,
		   var livesAtHome : Boolean = true) {
  
  def incrAge(){
    age += 1
  }
  
  
  def justDoneEducation : Boolean =
    	  age == education + 1
  def doneEducation :  Boolean = 
		  age >= education + 1
  
  
  override def toString : String =  
    "Sim(age=%d, education=%d, income=%f, health=%f, livesAtHome=%s" format
    (age, education, income, health, livesAtHome.toString())  
	
}

object Sim {
  
  /*
   * Various model constants
   */
  
  val MAX_HEALTH = 1.0
  val MIN_HEALTH = 0.0
  
  
  /*
   * Standard model functions (formulas, essentially)
   */
		  
  def computeEducation(parentalIncome : Double, avgNbhdIncome : Double) : Int
		  = 20 // TODO add real formula
  
		  
  
		  
  
  
  /**
   * create a new Sim based on its parent
   */
  def generateChild(parent : Sim, avgNbhdIncome: Double)  = 
     new Sim(Some(parent), 
    		 computeEducation(parent.income, avgNbhdIncome),
    		 parent.nbhd) // child is placed in the same neighborhood as its parent 
		  
		  
  
		  
  
}