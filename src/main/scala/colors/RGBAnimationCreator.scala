
package scala.colors

import java.awt.Color
import java.util

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

  val SIDE = 64

  val HEIGHT : Int = 256
  val WIDTH : Int = 256
  val DEPTH : Int = 256

  //val NUMCOLORS =  SIDE
  
  val sorted : Boolean = true

  val sortString = if(sorted) "_sorted" else "_not_sorted"

  val outputVideoFileName = "video_" + HEIGHT + "_" + WIDTH + "_" + DEPTH + "_" +
    sortString + ".mp4"
  
  val AVERAGE = false
  
  val MAXAVERAGE = 1000

  log.info("initiating voxel map")
  
  val voxels = new VoxelMap(HEIGHT, WIDTH, DEPTH)

  // we store the available voxels in two collections, the set is just to check
  // that we don't add a voxel that has already been added, the vector is so
  // that we can efficiently sample from the available voxels (always inefficient
  // without indexing it would seem)

  val available_set = HashSet[Voxel]()
  var available_vector = Vector[Voxel]()
  def remove[A](xs: Vector[A], i: Int) = (xs take i) ++ (xs drop (i + 1))

  val assigned = HashSet[Voxel]()

  val SEEDS = 1
  
//  val colors : List[Color] = if(sorted) {
//    ColorUtils.getColorListRankedByHue(NUMCOLORS)
//    } else {
//      ColorUtils.getRandomColorList(NUMCOLORS)
//    }

  val colors : List[Color] = if(sorted) {
    ColorUtils.getColorListRankedByHue(HEIGHT, WIDTH, DEPTH)
  } else {
    ColorUtils.getRandomColorList(HEIGHT, WIDTH, DEPTH)
  }

  val random = new Random(System.currentTimeMillis())
  
  val annealingBoolean = false
  
  def main(args : Array[String]){
    log.info("beginning...")
    log.info(s"image dimensions height : $HEIGHT, width : $WIDTH, DEPTH $DEPTH")
    log.info(s"colours to assign : ${colors.size}")

    val randomVoxels = getRandomVoxels(SEEDS)
    available_set.++=(randomVoxels)
    available_vector = available_vector ++(randomVoxels)


    var iter = 0
    for(thisColor <- colors.take(HEIGHT * WIDTH * DEPTH - 1)){

        val index = getBestAvailablePixel(thisColor)
        assignPixelColour(index, thisColor)
        iter +=1
        if(iter%5000==0){
        	log.info("assigned " + iter  + " colors, available  : " + available_vector.size)
        }
    }
    
    val writer = new AnimationWriter(voxels, "images")

    writer.writeMP4Video(outputVideoFileName)
    
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

  // we unroll this argmax for efficiency
  def getBestAvailablePixel(color : Color) : Int = {
    var bestScore = Int.MaxValue
    var bestIndex : Int = 0

    val size = available_vector.size


    for(i <- 0 until MAXAVERAGE){
      val index = random.nextInt(size)
      val vxl = available_vector(index)
      val score = calculateFitness(vxl, color)
      if (score < bestScore) {
        bestScore = score
        bestIndex = index
      }
    }

//    for (vxl <- available_set) {
//      if (random.nextFloat < thresholds) {
//        val score = calculateFitness(vxl, color)
//        if (score < bestScore) {
//          bestScore = score
//          bestVoxel = vxl
//        }
//      }
//    }
    return bestIndex
  }


//    val randomAvail = getRandomAvailableSubset()
//	  def compFitness(voxel : Voxel) = {
//		  calculateFitness(voxel, color)
//	  }
//     randomAvail.toSeq.sortBy(compFitness).head
//   }
//
//  def getRandomAvailableSubset() : Seq[Voxel] = {
//    if(available.size < MAXAVERAGE) available.toSet else {
//      val thresholds = MAXAVERAGE.toFloat / available.size
//      available.filter(_ => random.nextFloat < thresholds)
//    }
//
//  }
  
  def assignPixelColour(index : Int, color:Color) {
    val voxel = available_vector(index)
    voxel.setColor(color, false)
    //available_set.-=(voxel)
    available_vector = remove(available_vector, index)
    assigned.+=(voxel)
    
    for(neighbour <- voxels.getNeighBours(voxel, WRAP)){
      if(!assigned.contains(neighbour) && !available_set.contains(neighbour)){
        available_set.+=(neighbour)
        available_vector = available_vector :+(neighbour)
      }
      neighbour.adjustColorCache(color)      
    }    
  }
  
}

object RemoveTest {

  val SIZE = 40000

  val random = new Random()
  val log = Logger.getLogger(this.getClass)



  def remove[A](xs: Vector[A], i: Int) = (xs take i) ++ (xs drop (i + 1))

  def selectRandom1(h : HashSet[Int]): Unit ={
    var count = 0
    val threshold = 1000.0 / SIZE
    for(int <- h){
      if(random.nextFloat() < threshold){
        count += int
      }
    }
  }

  def selectRandom2(v : Vector[Int]): Unit ={
    var count = 0
    for(i <- 0 until 1000){

      count += v(random.nextInt(SIZE))
    }
  }


  def main(args : Array[String]): Unit ={


    val h = HashSet[Int]()
    var v = Vector[Int]()

    for(i <- 0 until SIZE){
      h.add(i)
      v = v :+(i)
    }

    val t00 = System.nanoTime()

    for(i <- 0 until 100){
      selectRandom1(h)
    }

    val t01 = System.nanoTime()

    log.info(t01 - t00)

    val t10 = System.nanoTime()

    for(i <- 0 until 100){
      selectRandom2(v)
    }

    val t11 = System.nanoTime()

    log.info(t11 - t10)

    val indices = (0 until SIZE).toList.reverse
    val random_indices = indices.map(n => random.nextInt(n + 1))

    log.info(random_indices.take(10))
    log.info(random_indices.drop(random_indices.size - 10))


    var t0 = System.nanoTime()

    for(i <- h){
      h -= i
    }

    var t1 = System.nanoTime()
    log.info(t1 - t0)

    t0 = System.nanoTime()
    for(index <- indices){
      v = remove(v, index)
    }
    t1 = System.nanoTime()
    log.info(t1 - t0)

  }


}
