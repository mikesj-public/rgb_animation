package scala.colors

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{FileOutputStream, BufferedOutputStream, OutputStream}
import javax.imageio.ImageIO

import main.scala.colors.Voxel
import org.apache.log4j.Logger
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.util
import util.Random




class VoxelMap(val height : Int, val width : Int, val depth : Int) {
  
  val voxels : Array[Array[Array[Voxel]]] = initialiseVoxelArray

  def initialiseVoxelArray() : Array[Array[Array[Voxel]]] = {
    val array = new Array[Array[Array[Voxel]]](height)
    for(rowNum <- 0 until height){
      val newRow = new Array[Array[Voxel]](width)
      for(colNum <- 0 until width){
        val newArray = new Array[Voxel](depth)
        for (timeNum <- 0 until depth){
          newArray(timeNum) = new Voxel(colNum, rowNum, timeNum)
        }
        newRow(colNum) = newArray
      }
      array(rowNum) = newRow
    }
    array
  }

  def getNeighBours(voxel : Voxel, wrap : Boolean) : List[Voxel] = {
    val buffer = new ListBuffer[Voxel]
    for{
      xCoord <- -1 to 1
      yCoord <- -1 to 1
      zCoord <- -1 to 1
      xNew = voxel.x + xCoord
      yNew = voxel.y + yCoord
      zNew = voxel.z + zCoord
      if(0 <= xNew  && xNew < width )
      if(0 <= yNew  && yNew < height)
      if(wrap || (0 <= zNew  && zNew < depth))
      if(0 != xNew || 0!= yNew || 0 != zNew)
    } yield {
      if(!wrap) buffer.+=(voxels(yNew)(xNew)(zNew))
      else buffer.+=(voxels(yNew)(xNew)(zNew % depth))

    }
    buffer.toList
  }

  def getVoxel(x : Int, y : Int, z:Int) = voxels(y)(x)(z)

  def getPNG(z : Int): BufferedImage = {
    val img : BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for{
      x <- 0 until width
      y <- 0 until height
    } yield {
      val rgb = getVoxel(x, y, z).getColor.getRGB()
      img.setRGB(x, y, rgb)
    }
    img
  }
}

object VoxelMap {

  val log = Logger.getLogger(this.getClass)

  val rand = new util.Random()

  def main(args : Array[String]): Unit ={

    val HEIGHT = 128
    val WIDTH = 128
    val DEPTH = 128

    val voxelMap = new VoxelMap(HEIGHT, WIDTH, DEPTH)

    val voxels = voxelMap.voxels

    val vxl = voxels(34)(35)(50)

    log.info(voxelMap.getNeighBours(vxl, true))

    log.info(voxels.length)
    log.info(voxels(0).length)
    log.info(voxels(0)(0).length)


    for(i <- 0 until 1000){
      val x = rand.nextInt(HEIGHT)
      val y = rand.nextInt(WIDTH)
      val z = rand.nextInt(DEPTH)

      //log.info("x : " + x + " ,y : " + y + " ,z : " + z)

      val r = rand.nextInt(256)
      val g = rand.nextInt(256)
      val b = rand.nextInt(256)

      val color = new Color(r, g, b)

      voxels(x)(y)(z).setColor(color, true)

    }

    for(i <- 0 until DEPTH) {

      val outputFileName = "images/img000" + i + ".png"

      val out: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFileName))

      ImageIO.write(voxelMap.getPNG(i), "png", out)
    }

  }


}