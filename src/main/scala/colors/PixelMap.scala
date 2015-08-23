package main.scala.colors

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import java.awt.image.BufferedImage

class PixelMap(val height : Int, val width : Int) {
  
  val pixels : Array[Array[Pixel]] = initialisePixelArray
  
  def initialisePixelArray() : Array[Array[Pixel]] = {
    val array = new Array[Array[Pixel]](height)
    for(rowNum <- 0 until height){
      val newRow = new Array[Pixel](width)
      for(colNum <- 0 until width){
        newRow(colNum) = new Pixel(colNum, rowNum)
      }
      array(rowNum) = newRow
    }    
    array
  }
  
  def getNeighBours(xy : Pixel) : List[Pixel] = {
    val buffer = new ListBuffer[Pixel]
    for{
      xCoord <- -1 to 1
      yCoord <- -1 to 1
      xNew = xy.x + xCoord
      yNew = xy.y + yCoord
      if(0 <= xNew  && xNew < width )
      if(0 <= yNew  && yNew < height)
      if(0 != xNew || 0!= yNew)
    } yield {
      buffer.+=(pixels(yNew)(xNew))
    }
    buffer.toList
  }
  
  def getPixel(x : Int, y : Int) = pixels(y)(x)
  
  def getPNG : BufferedImage = {
    val img : BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for{
      x <- 0 until width
      y <- 0 until height
    } yield {
      val rgb = getPixel(x, y).getColor.getRGB()
      img.setRGB(x, y, rgb)
    }
    img
  }   
}