package scala.colors

import java.io._
import javax.imageio.ImageIO

import main.scala.colors.PixelMap
import org.apache.log4j.Logger
import org.jcodec.common.NIOUtils
import org.jcodec.common.model.Size
import org.jcodec.containers.mp4.{MP4Packet, TrackType, Brand}
import org.jcodec.containers.mp4.muxer.MP4Muxer


class ImageWriter(pixels : PixelMap, outputFileName : String) {

  val log : Logger = Logger.getLogger(this.getClass())



  def writeImage {
    log.info(s"attempting to write to file $outputFileName")
    try {
      val out: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFileName))

      ImageIO.write(pixels.getPNG, "png", out);
    } catch {
      case e: IOException => e.printStackTrace()
    }

  }
}

class AnimationWriter(voxelMap : VoxelMap, dirName : String){

  val log : Logger = Logger.getLogger(this.getClass())

  val depth = voxelMap.depth



  def writeMP4Video(fileName : String) {

    log.info(s"attempting to write $depth images to directory $dirName")



    val fileNames : Seq[File] = for{i <- 0 until depth} yield {

      val paddedNum = "%04d".format(i)

      val outputFileName = dirName + "/img" + paddedNum + ".png"

      log.info(outputFileName)

      val out: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFileName))

      ImageIO.write(voxelMap.getPNG(i), "png", out)

      new File(outputFileName)
    }

    val out = new File(fileName)

    val writer = new SequenceMuxer(out)
    writer.writeImages(fileNames)

  }

}

class SequenceMuxer(val out : File) {

  private val log = Logger.getLogger(this.getClass)

  private var ch = NIOUtils.writableFileChannel(out)
  //private var outTrack : CompressedTrack = null

  private val muxer = new MP4Muxer(ch, Brand.MP4)

  private val outTrack = muxer.addTrackForCompressed(TrackType.VIDEO, 25)
  private var size: Size = null

  log.info("done.")


  def writeImages(fileNames : Seq[File]): Unit ={

    val size = getSize(fileNames.head)

    var frameNo: Int = 0

    for(png <- fileNames) {

      outTrack.addFrame(new MP4Packet(NIOUtils.fetchFrom(png), frameNo, 25, 1, frameNo, true, null, frameNo, 0))
      frameNo += 1
    }

    log.info(frameNo)

    outTrack.addSampleEntry(MP4Muxer.videoSampleEntry("png ", size, "JCodec"));

    // Write MP4 header and finalize recording
    muxer.writeHeader();
    NIOUtils.closeQuietly(ch);

  }

  def getSize(file : File): Size ={

    val read = ImageIO.read(file)
    return  new Size(read.getWidth(), read.getHeight());

  }
}
