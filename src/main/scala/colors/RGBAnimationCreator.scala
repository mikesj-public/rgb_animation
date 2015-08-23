
package scala.colors

import java.awt.Color

import org.apache.log4j.Logger

import scala.collection.mutable.HashSet
import scala.util.Random
import main.scala.colors.{ColorUtils, Voxel}

object RGBAnimationCreator {
  
  val log : Logger = Logger.getLogger(this.getClass)  
  

  
  val NUMCOLORSEXP = 6

  val WRAP = false
  
  
  
//  val HEIGHT : Int = Math.pow(2, Math.floor((NUMCOLORSEXP * 3).toDouble/2)).toInt
//  val WIDTH : Int = Math.pow(2, Math.ceil((NUMCOLORSEXP * 3).toDouble/2)).toInt

  val SIDE = 128

  val HEIGHT : Int = SIDE
  val WIDTH : Int = SIDE
  val DEPTH : Int = SIDE

  val NUMCOLORS =  SIDE
  
  val sorted : Boolean = false

  val sortString = if(sorted) "_sorted" else "_not_sorted"

  val outputVideoFileName = "video_" + SIDE + sortString + ".mp4"
  
  val AVERAGE = false
  
  val MAXAVERAGE = 1000
  
  val voxels = new VoxelMap(HEIGHT, WIDTH, DEPTH)
  
  val available = HashSet[Voxel]()
  
  val assigned = HashSet[Voxel]()

  val SEEDS = 1
  
  val colors : List[Color] = if(sorted) {
    ColorUtils.getColorListRankedByHue(NUMCOLORS)
    } else {
      ColorUtils.getRandomColorList(NUMCOLORS)
    }
  
  val random = new Random(System.currentTimeMillis())
  
  val annealingBoolean = false
  
  def main(args : Array[String]){
    log.info("beginning")
    log.info(s"image dimensions height : $HEIGHT, width : $WIDTH, DEPTH $DEPTH")
    log.info(s"colours to assign : ${colors.size}")
    
    available.++=(getRandomVoxels(SEEDS))
    var iter = 0
    for(thisColor <- colors.take(NUMCOLORS * NUMCOLORS * NUMCOLORS - 1)){
        val voxel = getBestAvailablePixel(thisColor)
        assignPixelColour(voxel, thisColor)
        iter +=1
        if(iter%5000==0){
        	log.info("assigned " + iter  + " colors, available  : " + available.size)
        }
    }
    
    val writer = new AnimationWriter(voxels, "images")

    writer.writeMP4Video("animation.mp4")
    
    log.info("finished")
    
  }
  
  def getRandomVoxels(n : Int) : Set[Voxel]= {
    val resultSet = new HashSet[Voxel]()
    for(i <- 0 until n) {
      resultSet.+=(getRandomVoxel)
    }
    resultSet.toSet
  }

  def getRandomVoxel() = {
    voxels.getVoxel(random.nextInt(WIDTH),random.nextInt(HEIGHT), random.nextInt(DEPTH))    
  }
  
  def calculateFitness(voxel : Voxel, color : Color) : Int = {
    ColorUtils.getColorDiff(color, voxel)
  }

  def getBestAvailablePixel(color : Color) : Voxel = {
    val randomAvail = getRandomAvailableSubset()
	  def compFitness(voxel : Voxel) = {
		  calculateFitness(voxel, color)
	  }    
     randomAvail.toSeq.sortBy(compFitness).head     
   }
  
  def getRandomAvailableSubset() : Set[Voxel] = {
    if(available.size < MAXAVERAGE) available.toSet else {
      val thresholds = MAXAVERAGE.toFloat / available.size
      available.filter(_ => random.nextFloat < thresholds).toSet
    }
    
  }
  
  def assignPixelColour(voxel : Voxel, color:Color) {
    voxel.setColor(color, false)
    available.-=(voxel)
    assigned.+=(voxel)
    
    for(neighbour <- voxels.getNeighBours(voxel, WRAP)){
      if(!assigned.contains(neighbour)){
        available.+=(neighbour)
      }
      neighbour.adjustColorCache(color)      
    }    
  }
  
}
