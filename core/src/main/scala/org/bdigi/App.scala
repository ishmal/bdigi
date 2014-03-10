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

import scala.beans.BeanProperty

import org.bdigi.mode._


//########################################################################
//# M A I N    D E F I N I T I O N S
//########################################################################

/**
 * These are global constants that would be shared in all
 * modules
 */  
object Constants
{
    /**
     * The low end of our desired band
     */
    val lowFrequency = 0.0
    
    /**
     * The high bound of our desired band
     */                  
    val highFrequency = 2500.0
    
    /**
     * The width of our desired band
     */                  
    val bandwidth = (highFrequency - lowFrequency)
    
}



/**
 * These properties define the main parent of any application.
 * Extends this to be the top of the object tree for any GUI,
 * command-line application, or service.  
 */ 
class App
{
    val self = this
		
    //########################################
    //# Logging
    //########################################
    
    def trace(msg: String) =
        {
        println("App: " + msg)
        }

    def error(msg: String) =
        {
        println("App error: " + msg)
        }

    def error(msg: String, e: Throwable)
        {
        println("App error: " + msg + " : " + e)
        }


    //########################################
    //# Devices
    //########################################

    
    var inputDevice  : Option[AudioInputDevice]  = None

    def setInputDevice(deviceName: String) =
        {
        inputDevice.foreach(_.close)
        val newdev = AudioDevice.createInput(this, deviceName)
        if (newdev.isDefined)
            {
            inputDevice.foreach(_.close)
            inputDevice = newdev
            inputDevice.get.open
            adjust
            }
        }
    
    var outputDevice : Option[AudioOutputDevice] = None

    def setOutputDevice(deviceName: String) =
        {
        outputDevice.foreach(_.close)
        val newdev = AudioDevice.createOutput(this, deviceName)
        if (newdev.isDefined)
            {
            outputDevice.foreach(_.close)
            outputDevice = newdev
            outputDevice.get.open
            adjust
            }
        }
    
    //44100.0 / 6
    @BeanProperty
    def sampleRate =
        7350.0
		
    val wf = new WaterfallFactory(this, 3072,  sampleRate, Constants.highFrequency)

	def getFrequency =
	    mode.frequency

    def setFrequency(f: Double) =
        mode.frequency = f
        
    @BeanProperty
    var rxtx = false
	
    def setAgc(v: Boolean) =
        mode.useAgc = v
    
    def getAgc : Boolean =
	    mode.useAgc
	
    //########################################
    //# Modes
    //########################################

	val nullMode   = new NullMode(this)
	val packetMode = new PacketMode(this)
    val pskMode    = new Psk31(this)
    val rttyMode   = new Rtty(this)
    val navtexMode = new Navtex(this)
    
    @BeanProperty
    val modes = Array(nullMode, packetMode, pskMode, rttyMode, navtexMode)
	
	@BeanProperty
	var mode : Mode = nullMode
    
    @BeanProperty
    def bandwidth =
        mode.bandwidth
        
    

    //########################################
    //# Settings
    //########################################
    
    
    class Config() 
        {
        private val propFile = "bdigi.ini"
    
        var call              = ""
        var name              = ""
        var qth               = ""
        var locator           = ""
        var audioInputDevice  = ""
        var audioOutputDevice = ""

		def load(ins:  java.io.InputStream) =
			{
			val props = Properties.load(ins)
			if (props.isDefined)
				{   
				val p = props.get
				call              = p("call")
				name              = p("name")
				locator           = p("locator")
				audioInputDevice  = p("audioInputDevice")
				setInputDevice(audioInputDevice)
				audioOutputDevice = p("audioOutputDevice")
				setOutputDevice(audioOutputDevice)
				}
			}

		def save(outs: java.io.OutputStream) =
			{
			val p = Map(
				"call" -> call,
				"name" -> name,
				"locator" -> locator,
				"audioInputDevice" -> audioInputDevice,
				"audioOutputDevice" -> audioOutputDevice
				)
			if (!Properties.save(p, outs))
				{
				error("configSave failed")
				}
			}

        }
    
    val config = new Config

    def configLoad : Boolean =
        {
        try
            {
            val ins = new java.io.FileInputStream("bdigi.ini")
            config.load(ins)
            ins.close
			true
            }
        catch 
            {
            case e: Exception => error("configLoad failed: " + e)
                false
            }
        }

    def configSave : Boolean =
        {
        try
            {
            val outs = new java.io.FileOutputStream("bdigi.ini")
            config.save(outs)
            outs.close
            true
            }
        catch 
            {
            case e: Exception => error("configSave failed: " + e)
                false
            }
        }


    //########################################
    //# Startup
    //########################################
    
    
    class TRLoop extends Thread("digi-trloop")
    {
        def abort = 
            cont = false
            
        var cont = false
		        
        override def run =
            {
            cont = true
            while (cont)
                {
                if (rxtx)
                    {
                    doTx(this)
                    }
                else
                    {
                    doRx(this)
                    }
                }
            }
    }//TRLoop
    
    
    val decimator    = new FirResampler(6)
    val interpolator = new FirResampler(6)
    val txbuf = Array.ofDim[Double](512)
    var txptr = 0

    def doRx(loop: TRLoop) =
        {
        if (inputDevice.isDefined)
            {
            val res = inputDevice.get.read
            if (res.isEmpty)
                {
                //trace("empty")
                //cont = false
                }
            else
                {
                //trace("ok")
                for (v <- res.get)
                    {
                    decimator.decimate(v)(iv =>
                        {
                        wf.update(iv)(ps => updateSpectrum(ps))
                        mode.receive(iv)
                        })
                    }
                }
            }
        }
    
    
    def doTx(loop: TRLoop) =
        {
        def upAndOut(buf: Array[Double]) =
            {
            for (v <- buf)
                {
                interpolator.interpolate(v)( iv=>
                    {
                    txbuf(txptr) = iv
                    txptr += 1
                    if (txptr >= txbuf.size)
                        {
                        //trace("data:" + txbuf)
                        outputDevice.get.write(txbuf)
                        txptr = 0
                        }
                    })
                }
            }
        val preamble = mode.getTransmitBeginData
        if (preamble.isDefined)
            outputDevice.get.write(preamble.get)
        while (loop.cont && outputDevice.isDefined)
            {
            val res = mode.getTransmitData
            if (res.isEmpty)
                {
                loop.cont = false
                }
            else
                {
                }
            }
        val postamble = mode.getTransmitEndData
        if (postamble.isDefined)
            outputDevice.get.write(postamble.get)
        }
        


    var trloop = new TRLoop
    
    def startProcessing =
        {
        trloop.abort
        trloop = new TRLoop
        trloop.start
        }

    def stopProcessing =
        {
        inputDevice.foreach(_.close)
        outputDevice.foreach(_.close)
        trloop.abort
        }  


    /**
     * Let's set things up
     */
    configLoad

    

    /**
     * Override these in your client code, especially for a GUI
     */


    def status(msg: String) =
        {}
    
    def puttext(msg: String) =
        {}
    
    def gettext : String =
        ""
    def updateScope(x: Double, y: Double) =
        {}

    def updateSpectrum(pixels: Array[Int]) =
        {}
    
    def adjust =
        {}
		

}//App






