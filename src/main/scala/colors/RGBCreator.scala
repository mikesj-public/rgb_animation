package main.scala.colors

import java.awt.Color
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.OutputStream
import javax.imageio.ImageIO
import java.io.IOException

import collection.mutable.HashSet
import scala.colors.ImageWriter
import util.Random
import org.apache.log4j.Logger

object RGBCreator {
  
  val log : Logger = Logger.getLogger(RGBCreator.this.getClass)  
  
  val outputFileName = "outputFile.png"
  
  val NUMCOLORSEXP = 6
  
  val NUMCOLORS =  Math.pow(2, NUMCOLORSEXP).toInt
  
  val HEIGHT : Int = Math.pow(2, Math.floor((NUMCOLORSEXP * 3).toDouble/2)).toInt
  val WIDTH : Int = Math.pow(2, Math.ceil((NUMCOLORSEXP * 3).toDouble/2)).toInt
  
  val sorted : Boolean = false
  
  val AVERAGE = false
  
  val MAXAVERAGE = 1000
  
  val pixels = new PixelMap(HEIGHT, WIDTH)
  
  val available = HashSet[Pixel]()
  
  val assigned = HashSet[Pixel]()
  
  val colors : List[Color] = if(sorted) {
    ColorUtils.getColorListRankedByHue(NUMCOLORS)
    } else {
      ColorUtils.getRandomColorList(NUMCOLORS)
    }
  
  val random = new Random(System.currentTimeMillis())
  
  val annealingBoolean = false
  
  def main(args : Array[String]){
    log.info("beginning")
    log.info(s"image dimensions height : $HEIGHT, width : $WIDTH")
    
    available.++=(getRandomPixels(1))
    var iter = 0
    for(thisColor <- colors.take(NUMCOLORS * NUMCOLORS * NUMCOLORS - 1)){
        val xy = getBestAvailablePixel(thisColor)
        assignPixelColour(xy, thisColor)      
        iter +=1
        if(iter%5000==0){
        	log.info("assigned " + iter  + " colors, available  : " + available.size)
        }
    }
    
    if(annealingBoolean){     
       val annealer = new SimulatedAnnealingSwapper(pixels)
       annealer.runAnnealing(10000000)
       log.info("finished annealing")
     }
    
    new ImageWriter(pixels, outputFileName).writeImage
    
    log.info("finished")
    
  }
  
  def getRandomPixels(n : Int) : Set[Pixel]= {
    val resultSet = new HashSet[Pixel]()
    for(i <- 0 until n) {
      resultSet.+=(getRandomPixel)
    }
    resultSet.toSet
  }

  def getRandomPixel() = {
    pixels.getPixel(random.nextInt(WIDTH),random.nextInt(HEIGHT))    
  }
  
  def calculateFitness(xy : Pixel, color : Color) : Int = {
    ColorUtils.getColorDiff(color, xy)
  }
  
  
  def getBestAvailablePixel(color : Color) : Pixel = {
    val randomAvail = getRandomAvailableSubset()
	  def compFitness(xy1 : Pixel) = {
		  calculateFitness(xy1, color)
	  }    
     randomAvail.toSeq.sortBy(compFitness).head     
   }
  
  def getRandomAvailableSubset() : Set[Pixel] = {
    if(available.size < MAXAVERAGE) available.toSet else {
      val thresholds = MAXAVERAGE.toFloat / available.size
      available.filter(_ => random.nextFloat < thresholds).toSet
    }
    
  }
  
  def assignPixelColour(xy : Pixel, color:Color) {
    xy.setColor(color, false)
    available.-=(xy)
    assigned.+=(xy)
    
    for(neighbour <- pixels.getNeighBours(xy)){
      if(!assigned.contains(neighbour)){
        available.+=(neighbour)
      }
      neighbour.adjustColorCache(color)      
    }    
  }
  
}

