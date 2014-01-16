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
    //# Devices
    //########################################

    
    var inputDevice  : Option[AudioInputDevice]  = None

    def setInputDevice(deviceName: String) =
        {
        val newdev = AudioDevice.createInput(this, deviceName)
        if (newdev.isDefined)
            {
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
        if (inputDevice.isDefined) inputDevice.get.sampleRate else 7350.0
        }
		
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
    
    class Config 
        {
        private val propFile = "sdr.ini"
    
        var call              = ""
        var name              = ""
        var locator           = ""
        var audioInputDevice  = ""
        var audioOutputDevice = ""

		def load =
			{
			val props = Properties.loadFile(propFile)
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
				outputDevice      = AudioDevice.createOutput(self, audioOutputDevice)
				outputDevice.foreach(_.open)
				}
			}

		def save =
			{
			val p = Map(
				"call" -> call,
				"name" -> name,
				"locator" -> locator,
				"audioInputDevice" -> audioInputDevice,
				"audioOutputDevice" -> audioOutputDevice
				)
			if (!Properties.saveFile(p, propFile))
				{
				Log.error("configSave failed")
				}
			}

        }
    
    val config = new Config


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
            while (cont && inputDevice.isDefined)
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
                        updateSpectrum(v)
					    mode.receive(v)
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
                    //Log.trace("data:" + res.get.size)
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
    config.load
    
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

    def updateSpectrum(data: Double) =
        {}
    
    def adjust =
        {}
		

}//App






