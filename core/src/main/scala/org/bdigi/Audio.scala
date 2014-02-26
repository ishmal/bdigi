/**
 * Scala SDR tool
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2014 Bob Jamison
 * 
 *  This file is part of the Scala SDR library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package org.bdigi


import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, 
    Line, Mixer, Port, SourceDataLine, TargetDataLine}





/**
 * Base class for all audio devices
 */
trait AudioDevice
{
    def sampleRate : Double
    def open : Boolean
    def close : Boolean
}


/**
 * Common aspects of all types of audio output devices (Java, Android, etc)
 */
trait AudioInputDevice extends AudioDevice
{
    def read : Option[Array[Double]]
}


/**
 * Common aspects of all types of audio output devices (Java, Android, etc)
 */
trait AudioOutputDevice extends AudioDevice
{
    def write(buf: Array[Double]) : Boolean
}



/**
 * Implementation of an AudioInputDevice on the JVM
 */
class JavaAudioInput(par: App, adi: AudioDeviceInfo) extends AudioInputDevice
{
    private val line            = AudioSystem.getTargetDataLine(adi.format, adi.mixerInfo)
    private val frameSize       = adi.format.getFrameSize
    private val framesPerBuffer = line.getBufferSize / 8
    private val bufsize         = 2048 * frameSize
    //trace("frameSize: " + frameSize + "  bufsize: " + bufsize)
    private val buf             = Array.ofDim[Byte](bufsize*2)
    private val vbuf            = Array.ofDim[Double](bufsize)


    
    val resampler = new FirResampler(6)
    

    def sampleRate =
        7350.0
        
    
    def open : Boolean =
        {
        line.open(adi.format, bufsize)
        line.start
        true
        }
        
    def close : Boolean =
        {
        line.stop
        line.close
        true
        }
        
    val doubleToShort  = 32767.0
    val shortToDouble  = 1.0 / 32768.0
    
    val bytesToDouble =
        {
        val arr = Array.ofDim[Double](256, 256)
        for (hi <- 0 until 256)
            {
            for (lo <- 0 until 256)
                {
                val v = ((hi << 8) + lo).toDouble * shortToDouble
                arr(hi)(lo) = v
                }
            }
        arr
        }

    def read : Option[Array[Double]] =
        {
        val numBytes = line.read(buf, 0, bufsize)
        if (numBytes <= 0)
            {
            None
            }
        else
            {
            var vptr = 0
            for (i <- 0 until numBytes by 2)
                {
                val dval = bytesToDouble(buf(i) & 0xff)(buf(i+1) & 0xff)
                //par.trace("dval:" + dval)
                
                resampler.decimate(dval)( v =>
                    {
                    //par.trace("v:" + v)
                    vbuf(vptr) = v
                    vptr += 1
                    })
                }
            val packet = Array.ofDim[Double](vptr)
            System.arraycopy(vbuf, 0, packet, 0, vptr)
            Some(packet)
            }
        }
}




/**
 * Implementation of an AudioOutputDevice on the JVM
 */
class JavaAudioOutput(par: App, adi: AudioDeviceInfo) extends AudioOutputDevice
{
    private val line            = AudioSystem.getSourceDataLine(adi.format, adi.mixerInfo)
    private val frameSize       = adi.format.getFrameSize
    private val framesPerBuffer = line.getBufferSize / 8
    private val bufsize         = 4096 * frameSize
    private val buf             = Array.ofDim[Byte](bufsize)

    val resampler = new FirResampler(6) // 8000->44100


    def sampleRate =
        7350.0

    def open : Boolean =
        {
        line.open(adi.format, bufsize)
        line.start
        true
        }
        
    def close : Boolean =
        {
        line.close
        true
        }
        
    /*
     * What we expect is an array of doubles, -1.0 to 1.0
     */
    def write(inbuf: Array[Double]) : Boolean =
        {
        var bptr = 0
        for (ival <- inbuf)
            {
            resampler.interpolate(ival) ( v =>
                {
                val iv = (v * 32767.0).toInt
                val hi = ((iv >> 8) & 0xff).toByte
                val lo = ((iv     ) & 0xff).toByte
                //par.trace("iv:" + iv)
                buf(bptr) = hi
                bptr += 1
                buf(bptr) = lo
                bptr += 1
                if (bptr >= bufsize)
                    {
                    line.write(buf, 0, bufsize)
                    bptr = 0
                    }
                })
            }
        line.write(buf, 0, bptr)
        true
        }

}




/**
 * Data class describing available audio devices
 */
case class AudioDeviceInfo(format: AudioFormat, mixerInfo: Mixer.Info)



/**
 * Utility for listing and creating audio devices
 */
object AudioDevice
{
    /**
     * List conforming audio input devices
     */
    val inputDevices : Map[String, AudioDeviceInfo] =
        {
        val audioFormat = new AudioFormat(44100.0f, 16, 1, true, true)
        val info = new DataLine.Info(classOf[TargetDataLine], audioFormat)
        val buf = scala.collection.mutable.Map[String, AudioDeviceInfo]()
        for (mixerInfo <- AudioSystem.getMixerInfo)
            {
            val m = AudioSystem.getMixer(mixerInfo)
            if (m.isLineSupported(info))
                {
                buf +=  mixerInfo.getName -> AudioDeviceInfo(audioFormat, mixerInfo)
                }
             }
        buf.toMap   
        }


     
    /**
     * List conforming audio output devices
     */
    val outputDevices : Map[String, AudioDeviceInfo] =
        {
        val audioFormat = new AudioFormat(44100.0f, 16, 1, true, true)
        val info = new DataLine.Info(classOf[SourceDataLine], audioFormat)
        val buf = scala.collection.mutable.Map[String, AudioDeviceInfo]()
        for (mixerInfo <- AudioSystem.getMixerInfo)
            {
            val m = AudioSystem.getMixer(mixerInfo)
            if (m.isLineSupported(info))
                {
                buf +=  mixerInfo.getName -> AudioDeviceInfo(audioFormat, mixerInfo)
                }
             }
        buf.toMap   
        }
        

    /**
     * Create an audio input device by name.  If device is not in the list,
     * return an error
     */
    def createInput(par: App, name: String) : Option[AudioInputDevice] =
        {
        val dev = inputDevices.get(name)
        if (dev.isDefined)
            {
            Some(new JavaAudioInput(par, dev.get))
            }
        else
            {
            par.error("Input audio device not found: " + name)
            None
            }
        }
        
    /**
     * Create an audio output device by name .  If name does not exist, return an error
     */
    def createOutput(par: App, name: String) : Option[AudioOutputDevice] =
        {
        val dev = outputDevices.get(name)
        if (dev.isDefined)
            {
            Some(new JavaAudioOutput(par, dev.get))
            }
        else
            {
            par.error("Output audio device not found: " + name)
            None
            }
        }
}





