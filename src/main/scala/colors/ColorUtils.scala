package main.scala.colors

import java.awt.Color
import scala.collection.mutable.ListBuffer
import scala.util.Random

object ColorUtils {
  
  val random = new Random(System.currentTimeMillis())
  
  def getColorDiff(c1 : Color, c2 : Color) : Int = {
    val r = c1.getRed - c2.getRed
    val b = c1.getBlue - c2.getBlue
    val g = c1.getGreen - c2.getGreen
    r*r + b*b + g*g
  }
  
  def getColorDiff(c : Color, cell : Cell) : Int = {
    val r = c.getRed()
    val g = c.getGreen()
    val b = c.getBlue()
    
    cell.getColorSquareSum -
    	2 * r * cell.getRedSum -
    	2 * g * cell.getGreenSum -
    	2 * b * cell.getBlueSum
  }
  
  def getRandomColorList(numColors : Int) : List[Color] = {
    val colors = getColorList(numColors)
    random.shuffle(colors).toList
  }
  
  def getColorList(numColors : Int) : List[Color] = {
    val colors =  new ListBuffer[Color]
    for {
      r <- 0 until numColors
      b <- 0 until numColors
      g <- 0 until numColors      
    } yield {
      colors.+=(new Color(
          (r * 255f / (numColors -1)).toInt, 
          (g * 255f / (numColors -1)).toInt,
          (b * 255f / (numColors -1)).toInt))
    }
    colors.toList
  }
  
  def getColorListRankedByHue(numColors : Int) : List[Color] = {
    val colors = getColorList(numColors)
    colors.sortBy(_.getRGB())
    
  }

}