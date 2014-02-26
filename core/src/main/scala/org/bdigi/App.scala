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
    val highFrequency = 2800.0
    
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
        val newdev = AudioDevice.createInput(this, deviceName)
        if (newdev.isDefined)
            {
            trace("input ok")
            receiver.abort
            inputDevice.foreach(_.close)
            inputDevice = newdev
            inputDevice.get.open
            receive
            adjust
            }
        }
    
    var outputDevice : Option[AudioOutputDevice] = None

    def setOutputDevice(deviceName: String) =
        {
        val newdev = AudioDevice.createOutput(this, deviceName)
        if (newdev.isDefined)
            {
            transmitter.abort
            outputDevice.foreach(_.close)
            outputDevice = newdev
            outputDevice.get.open
            transmit
            adjust
            }
        }
    

    def sampleRate =
        {
        val fs = if (inputDevice.isDefined) inputDevice.get.sampleRate else 7350.0
        //trace("fs:" + fs)
        fs
        }
		
    val wf = new WaterfallFactory(this, 2048,  sampleRate, 2500.0)

	def frequency =
	    mode.frequency

    def frequency_=(f: Double) =
        mode.frequency = f
        
    def rxtx_=(v: Boolean) =
        {
        if (v) transmit else transmitter.abort
        }
    
    def rxtx : Boolean =
	    {true}
	
    def agc_=(v: Boolean) =
        mode.useAgc = v
    
    def agc : Boolean =
	    mode.useAgc
	
    //########################################
    //# Modes
    //########################################

	val nullMode   = new NullMode(this)
	val packetMode = new PacketMode(this)
    val pskMode    = new Psk31(this)
    val rttyMode   = new Rtty(this)
    val navtexMode = new Navtex(this)
    
    val modes = List(nullMode, packetMode, pskMode, rttyMode, navtexMode)
	
	private var modeVal : Mode = nullMode

    def mode : Mode = modeVal
	
	def mode_=(v: Mode) =
		modeVal = v
		
    mode = nullMode
    
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
				inputDevice       = AudioDevice.createInput(self, audioInputDevice)
				inputDevice.foreach(_.open)
				audioOutputDevice = p("audioOutputDevice")
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
			inputDevice = AudioDevice.createInput(self, config.audioInputDevice)
			inputDevice.foreach(_.open)
    		outputDevice = AudioDevice.createOutput(self, config.audioOutputDevice)
			outputDevice.foreach(_.open)
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
    

    class Receiver extends Thread("sdr-receiver")
    {
        def abort = 
            cont = false
            
        var cont = false
		        
        override def run =
            {
            cont = true
            while (cont)
                {
                if (inputDevice.isDefined)
                    {
                    val res = inputDevice.get.read
                    if (res.isEmpty)
                        {
                        //cont = false
                        }
                    else
                        {
                        for (v <- res.get)
                            {
                            wf.update(v)(ps => updateSpectrum(ps))
                            mode.receive(v)
                            }
                        }
                    }
                }
            }
    }
    
    

    var receiver = new Receiver
    
    def receive =
        {
        receiver.abort
        receiver = new Receiver
        receiver.start
        }




    class Transmitter extends Thread("sdr-transmitter")
    {
        def abort = 
            cont = false
            
        var cont = false
		        
        override def run =
            {
            cont = true
            val preamble = mode.getTransmitBeginData
            if (preamble.isDefined)
                outputDevice.get.write(preamble.get)
            while (cont && outputDevice.isDefined)
                {
                val res = mode.getTransmitData
                if (res.isEmpty)
                    {
                    cont = false
                    }
                else
                    {
                    //trace("data:" + res.get.size)
                    outputDevice.get.write(res.get)
                    }
                }
            val postamble = mode.getTransmitEndData
            if (postamble.isDefined)
                outputDevice.get.write(postamble.get)
            }
    }
    
    var transmitter = new Transmitter
    
    def transmit =
        {
        transmitter.abort
        transmitter = new Transmitter
        transmitter.start
        }


    /**
     * Let's set things up
     */
    configLoad
    
    receive

    def stop =
        {
        receiver.abort  
        transmitter.abort
        inputDevice.foreach(_.close)
        outputDevice.foreach(_.close)
        }  


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






