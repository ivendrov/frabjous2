package ui

import scala.util.parsing.json._
import scala.io._
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import java.io.BufferedReader
import javax.swing._
import event._
import util.Random
import java.awt.Graphics
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Color
import java.lang.reflect.Field

abstract class Shape
case class Line(x1 : Int, y1 : Int, x2 : Int, y2 : Int) extends Shape
case class Circle(x : Int, y : Int, r : Int) extends Shape

case class DisplayObject(shape : Shape, color : Color)


class MyPanel(val h : Int, val w: Int) extends JPanel {
    var objects : List[DisplayObject] = List()
    var time : Double = 0
  

    override def getPreferredSize() : Dimension =  {
        return new Dimension(w, h);
    }

    override def paintComponent(g : Graphics) {
        super.paintComponent(g);  
        val g2 = g.asInstanceOf[Graphics2D]

        // Draw Text
        g.drawString("Time: " + time,10,20);
        
        // Draw objects
        for (obj <- objects){
          g2.setPaint(obj.color)
          obj.shape match {
            case Line(x1, y1, x2, y2) => g2.drawLine(x1, y1, x2, y2)
            case Circle(x,y,r) => g2.fillOval(x-r, y-r, 2*r, 2*r)
          }
        }

    }  
}






object Main extends App{
  
  
  
  
  val lines = Source.stdin.getLines()
  // parse each line as a json object
  
  type Obj = Map[String, Any]
  def stringToColor( str: String) = {
    val field : Field = classOf[Color].getField(str)
    field.get(null).asInstanceOf[Color]
  }
  def readObj(l : String) = JSON.parseFull(l).get.asInstanceOf[Obj]
  def readArray(obj : Obj, name : String) = obj.get(name).get.asInstanceOf[List[Obj]]
  def readDouble(obj : Obj, name : String) = obj.get(name).get.asInstanceOf[Double]
  def readInt(obj : Obj, name : String) = readDouble(obj, name).toInt
  def readString(obj : Obj, name : String) = obj.get(name).get.asInstanceOf[String]
  def readShape(obj: Obj) : DisplayObject = {
    val shape = if (readString(obj, "shape") == "line") 
    					Line (readInt(obj, "x1"), readInt(obj, "x1"), readInt(obj, "x1"), readInt(obj, "x1"))
    		    else 
    		    		Circle(readInt(obj, "x"), readInt(obj, "y"), readInt(obj, "r"))
    DisplayObject (shape, stringToColor(readString(obj, "color")))      
  }
  
  val fstLine = readObj(lines.next())
  val height = readDouble(fstLine, "height").toInt
  val width = readDouble(fstLine, "width").toInt
  val panel = new MyPanel(height, width)
  val frame = new JFrame("Model Visualization")
  frame.add(panel)
  frame.pack()
  frame.setVisible(true)
  
  
  for (l <- lines){
    val obj  = readObj(l)
    panel.time = readDouble(obj, "time")
    panel.objects = readArray(obj, "objects").map(readShape)
    frame.repaint()
  }
  
}