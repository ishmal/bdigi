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

package org.bdigi.mode


import org.bdigi._



/**
 * The base trait for any radio mode (psk, rtty, JT65, etc).
 *    
 * Any implementation of Mode will be parented by an App, and
 * should pass "val par: App" in its constructor. 
 */ 
class Mode(var par: App, val sampleRateHint: Double)
{
    def this(par: App) = this(par, par.sampleRate)
    
    
    def error(msg: String) = par.error(msg)

    def trace(msg: String) = par.trace(msg)

    /**
     * These two methods are used for starting or
     * stopping an experiment during development and have no purpose 
     * for the end-user.
     */                   
    def start =
        {}
    
    def stop =
        {}
        
    val name = "null"
    val tooltip = "None"
    val properties  = new PropertyGroup(name)
    
    /**
     * Call status(msg) for information that you might want displayed
     * to the user.
     */              
    def status(msg: String) =
        par.status(msg)


    protected var frequencyVal = 0.0
    
    def frequency =
        frequencyVal
        
        
    def frequency_=(v: Double) =
       {
       frequencyVal = v
       nco = new AdjustableNco(v, 20.0, par.sampleRate)
       //trace("freq:" + v + "  fs:"+par.sampleRate)
       }
        
    protected var nco = new AdjustableNco(frequency, 20.0, par.sampleRate)

    val twopi = 2.0 * math.Pi
        
    //convert frequency in hertz to radians per sample
    def toFreq(f: Double) =
        twopi * f / sampleRate
        
    val decimation = math.floor(par.sampleRate / sampleRateHint).toInt
    
    val sampleRate = par.sampleRate / decimation
    
    private val decimator    = new FirResampler(decimation)
    private val interpolator = new FirResampler(decimation)
    
    //trace("requested fs:" + sampleRateHint + "  decimation: " +
    //    decimation + "   sampleRate:" + sampleRate)
    
    
    def receive(iv: Double)
        {
        val cs = nco.next
        val vi =  iv * cs.r
        val vq = -iv * cs.i
        val incpx = Complex(vi, vq)
    
        decimator.decimate(incpx) ( cpx =>
            {  
            //trace("cpx: " + cpx)
            if (useAgc)
                {
                val v = update(agc.update(cpx))
                agc.feedback(v)
                }
            else
                {
                update(cpx)
                }
            })

        }
 

    /**
     * This method is central to the ability of the Mode to
     * receive.  Data is sent to the method as a sequence of
     * double-valued, signed audio samples with the audio source's
     * sampling rate. 
     * The function should return a measure of the sample's energy,
     * if using AGC.  If agc not desired, return 1.0        
     */                     
    def update(data: Complex) : Double = 1.0
    
        

    //#######################################
    //# Rate
    //#######################################    
    private var rateInHertz = 100.0
    
    def rate = rateInHertz
    
    /**
     * This is the data rate of the Mode.  This can be overridden
     * by an implementation for any specific processing.  This is
     * used a lot for filters
     */                   
    def rate_=(newrate: Double) =
        {
        rateInHertz = newrate
        rateChanged(newrate)
        }
        
    /**
     * Override this when you need to readjust things after a change
     */
    def rateChanged(v: Double) =
        {}

    /**
     * This is used for modulation and demodulation, and timing
     * and decoding.  Make sure sampleRate > rate!!
     */              
    def samplesPerSymbol =
        sampleRate / rateInHertz
        
        
 

    //#######################################
    //# Used to display information
    //#######################################   
    //override this for each mode 
    def bandwidth = rate
    
    val reticleOrientation = 0
    
    
    //###########################################################
    //# O T H E R    S E T T I N G S
    //###########################################################
    private var agc = new SlowAgc(10000.0, sampleRate)
        
    var useAgc = true

    var useAfc = false

    var useMl  = false


    
    private val continuousWave = Array.ofDim[Double](2048)
    
    /**
     * Increate data rate to "bus" speed, upmix to frequency
     */                                  
    def upmixTransmitData(data: Option[Array[Complex]]) : Option[Array[Double]] =
        {
        if (data.isEmpty)
            None
        else
            {
            val buf = scala.collection.mutable.ListBuffer[Double]()
            for (v <- data.get)
                {
                interpolator.interpolate(v) ( vv => 
                    {
                    val outv = vv * nco.next
                    val outvv = outv.r + outv.i
                    //trace("vv:" + outvv)
                    buf.append(outvv) 
                    })
                }
            Some(buf.toArray)
            }
        }
    
    /**
     * Override this for each mode.
     */                                  
    def transmitBegin : Option[Array[Complex]] =
        {
        None
        }
        
    def getTransmitBeginData : Option[Array[Double]] =
        upmixTransmitData(transmitBegin)
        
    /**
     * This is the inverse of update().  Return None at end-of-data.
     * Override this for each mode.
     */                                  
    def transmit : Option[Array[Complex]] =
        {
        None
        }
        
    def getTransmitData : Option[Array[Double]] =
        upmixTransmitData(transmit)
        
    /**
     * This is the inverse of update().  Return None at end-of-data.
     * Override this for each mode.
     */                                  
    def transmitEnd : Option[Array[Complex]] =
        {
        None
        }
        
    def getTransmitEndData : Option[Array[Double]] =
        upmixTransmitData(transmitEnd)        

}

class NullMode(par: App) extends Mode(par)
{
}



