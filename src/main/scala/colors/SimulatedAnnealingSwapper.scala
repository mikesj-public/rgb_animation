package main.scala.colors

import java.awt.Color
import scala.util.Random

class SimulatedAnnealingSwapper(pixelMap : PixelMap) {
  
  val DEPTH = 128
  
  val random = new Random(System.currentTimeMillis())
  
  val startTemperature = 0.1
  
  var TOTAL_ITERATIONS : Int = 0  
  
  var acceptedSwaps =0
  
  def runAnnealing(iterations : Int){
    TOTAL_ITERATIONS = iterations
    for(iter <- 0 until TOTAL_ITERATIONS){
      if(iter%100000 == 0) {
        println(s"annealing : $iter iterations, currently swapped : $acceptedSwaps swaps" )
      }
      
      val depth = random.nextInt(DEPTH)
      
      val swap = getRandomSwap(depth)
      if(acceptSwap(swap._1, swap._2, iter)) {
        swapColors(swap._1, swap._2)
        acceptedSwaps +=1
      }
    }
  }
  
  def swapColors(xy1 : Pixel, xy2 : Pixel) {
    val color1 = xy1.getColor
    xy1.setColor(xy2.getColor, true)
    xy2.setColor(color1, true)
  }
  
  def acceptSwap(xy1 : Pixel, xy2 : Pixel, iter : Int) : Boolean = {
    val threshold = Math.exp((-getCostOfSwap(xy1, xy2).toDouble / getTemperature(iter)))
    random.nextDouble < threshold
  }
  
  def getTemperature(iter : Int) : Double = {
    startTemperature * (TOTAL_ITERATIONS - iter) / TOTAL_ITERATIONS
  }
  
  def getCostOfSwap(xy1 : Pixel, xy2 : Pixel) : Int = {
    val c1 : Color = xy1.getColor
    val c2 : Color = xy2.getColor
    
    val originalCost = getCostOfPlace(xy1, c1) +
    	getCostOfPlace(xy2, c2)
    
    val newCost = getCostOfPlace(xy2, c1) +
    	getCostOfPlace(xy1, c2) 
    newCost - originalCost
  }
  
  def getCostOfPlace(xy : Pixel, color : Color) : Int = {
    var total = 0
    for(neighbour <- pixelMap.getNeighBours(xy)){
      total +=  ColorUtils.getColorDiff(color, neighbour)
    }
    total
  }
  
  
  def getRandomSwap(depth : Int) : (Pixel , Pixel)= {
	val startPixelX = random.nextInt(pixelMap.width)
    val startPixelY = random.nextInt(pixelMap.height)
    val startPixel = pixelMap.getPixel(startPixelX, startPixelY)
    
    val nextPixelCoords : (Int, Int) = getRandomNewPixel(depth, startPixelX, startPixelY)
    
    (pixelMap.getPixel(startPixelX, startPixelY), 
        pixelMap.getPixel(nextPixelCoords._1, nextPixelCoords._2))
  }
  
  def getRandomNewPixel(depth : Int,  startPixelX : Int , startPixelY : Int) : (Int, Int) = {
    var newPixelX : Int= -1
    var newPixelY : Int = -1
    
    while(newPixelX == -1) {
      val tryPixelX = getRandomIntFromInterval(startPixelX - depth, startPixelX + depth)
      if(tryPixelX >=0 && tryPixelX < pixelMap.width) {
        newPixelX = tryPixelX
      }
    }    
     while(newPixelY == -1) {
      val tryPixelY = getRandomIntFromInterval(startPixelY - depth, startPixelY + depth)
      if(tryPixelY >=0 && tryPixelY < pixelMap.height) {
        newPixelY = tryPixelY
      }
    }    
    (newPixelX, newPixelY)
  }
  
  def getRandomIntFromInterval(startInt : Int , endInt : Int) : Int = {
    random.nextInt(endInt - startInt + 1) + startInt
  }
  
  

}