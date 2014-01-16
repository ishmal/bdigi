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



/** TODO:  Everything.  Simply copied from another file as a placeholder */

/**
 * Mode for Radio teletype.  Sends a standard
 * async code with a start bit, 5 data bits and
 * a stop bit.  Whether a parity bit is sent or
 * interpreted should be adjustable.
 *  
 * @see http://en.wikipedia.org/wiki/Radioteletype
 * @see http://en.wikipedia.org/wiki/Asynchronous_serial_communication
 *   
 */    
class Mfsk(par: App) extends Mode(par, 250.0)
{
    var inverted = false
    
    private var shiftVal = 170.0
    
    def shift = shiftVal
    
    def shift_=(v: Double) =
        {
        shiftVal  = v
        adjust
        }
        
    override def rate_=(v: Double) =
        {
        super.rate = v
        adjust
        }
    
    override def bandwidth =
        shift
        
    var unshiftOnSpace = false
    
    rate      = 45.0
    shift     = 170.0
    
    val q = 0.1
    
    var markFreq  = 0.0
    var spaceFreq = toFreq(shift * 0.5)
    var mf = Biquad.lowPass(shift * 0.35, sampleRate, q)
    var sf = Biquad.highPass(shift * 0.65, sampleRate, q)
    //var dataFilter = Iir2.lowPass(rate, sampleRate)
    var dataFilter = Fir.boxcar(samplesPerSymbol.toInt)
    var txlpf = Fir.lowPass(31,  shift * 0.5, sampleRate)
    
    var avgFilter = Iir2.lowPass(rate / 100, sampleRate)


    def adjust =
        {
        markFreq  = 0.0
        spaceFreq = toFreq(shift * 0.5)
        mf = Biquad.lowPass(shift * 0.35, sampleRate, q)
        sf = Biquad.highPass(shift * 0.65, sampleRate, q)
        //dataFilter = Iir2.lowPass(rate, sampleRate)
        dataFilter = Fir.boxcar(samplesPerSymbol.toInt)
        txlpf = Fir.lowPass(31,  shift * 0.5, sampleRate)
        }
        
    

    status("sampleRate: " + sampleRate + " samplesPerSymbol: " + samplesPerSymbol)


    val loHys = -0.5
    val hiHys =  0.5

    var bit = false
    
    var pct = 0
    var sum = 0.0
    val debug = false

    var lastVal = Complex(0.0)
    
	/**
	 * note: multiplying one complex sample of an
	 * FM signal with the conjugate of the previous
	 * value gives the instantaneous frequency change of
	 * the signal.  This is called a polar discrminator.
	 */             
    override def update(sample:  Complex) : Double =
        {
		val prod   = sample * lastVal.conj
		lastVal    = sample
		val demod  = prod.arg
		val comp   = math.signum(demod) * 10.0
		val sig    = dataFilter.update(comp)
		//println("sig:" + sig + "  comp:" + comp)

		par.updateScope(sig, 0)

        //println("sig:" + sig)
		if (sig > hiHys)
		    {
			bit = true
            }
		else if (sig < loHys)
		    {
			bit = false
		    }

		process(bit)
		
		sig
        }

    
    var parityType : Parity = ParityNone

    def parityOf(c: Int) : Boolean =
        {
        parityType match
            {
            case ParityOdd  => (java.lang.Integer.bitCount( c ) & 1) != 0
            case ParityEven => (java.lang.Integer.bitCount( c ) & 1) == 0
            case ParityZero => false
            case ParityOne  => true
            case _          => false   //None or unknown
            }
        }
    

    trait RxState
    case object RxIdle   extends RxState
    case object RxStart  extends RxState
    case object RxStop   extends RxState
    case object RxStop2  extends RxState
    case object RxData   extends RxState
    case object RxParity extends RxState
    
    var state : RxState = RxIdle
    var counter   = 0
    var code      = 0
    var parityBit = false
    var bitMask   = 0
   
    def process(inbit: Boolean) =
        {
        val bit = inbit ^ inverted //LSB/USB flipping
        val symbollen = samplesPerSymbol.toInt

        state match
            {
            case RxIdle =>
                //trace("RxIdle")
                if (!bit)
                    {
                    state   = RxStart
                    counter = symbollen / 2
                    }
            case RxStart => 
                //trace("RxStart")
                counter -= 1
                //keep idling until half a period of mark has passed
                if (bit)
                    {
                    state = RxIdle
                    }
                else if (counter <= 0)
                    {
                    //half a period has passed
                    //still unset? then we have received a start bit
                    state     = RxData
                    counter   = symbollen
                    code      = 0
                    parityBit = false
                    bitMask   = 1
                    }
            case RxData => 
                //trace("RxData")
                counter -= 1
                if (counter <= 0)
                    {
                    if (bit) code += bitMask
                    bitMask <<= 1
                    counter = symbollen
                    }
                 if (bitMask >= 0x20)
                     {
                     if (parityType == ParityNone) // todo:  or zero or 1
                         {
                         state = RxStop
                         }
                     else
                         {
                         state = RxParity
                         }
                     }
            case RxParity => 
                //trace("RxParity")
                counter -= 1
                if (counter <= 0)
                    {
                    state     = RxStop
                    parityBit = bit
                    counter   = symbollen
                    }
            case RxStop =>
                //trace("RxStop")
                counter -= 1
                if (counter <= 0)
                    {
                    if (bit)
                        {
                        outCode(code)
                        }
                    state = RxStop2
                    counter = symbollen / 2
                    }
            case RxStop2 =>
                //trace("RxStop2")
                counter -= 1
                if (counter <= 0)
                    {
                    state = RxIdle
                    }
            }
        }
    
    var shifted = false
    
       
    def reverse(v: Int, size: Int) : Int =
        {
        var a = v
        var b = 0
        for (i <- 0 until size)
            {
            b += a & 1
            b <<= 1
            a >>= 1 
            }
        b
        }
    
    
    
    var cntr = 0
    var bitinverter = 0

    override def start =
        {
        bitinverter = cntr
        cntr += 1
        if (cntr >= 32)
            cntr = 0
        status("bitinverter: " + bitinverter)
        }
    
    override def stop =
        bitinverter = 0
    

    def cleanup(c: Int) : Int =
        {
        (c ^ bitinverter) & 0x1f        
        }


    def outCode(rawcode: Int) =
        {
        //println("raw:" + rawcode)
        val code = rawcode & 0x1f
        if (code != 0)
            {
            if (code == Baudot.BAUD_FIGS)
                shifted = true
            else if (code == Baudot.BAUD_LTRS)
                shifted = false
            else if (code == Baudot.BAUD_SPACE)
                {
                par.puttext(" ")
                if (unshiftOnSpace)
                    shifted = false
                }
            else if (code == Baudot.BAUD_CR || code == Baudot.BAUD_LF)
                {
                par.puttext("\n")
                if (unshiftOnSpace)
                    shifted = false
                }
            val v = Baudot.baudCodeToSym(code)
            val c = if (shifted) v._2 else v._1
            if (c != 0)
                par.puttext(c.toChar.toString)
            }
            
        }
    
    //################################################
    //# T R A N S M I T
    //################################################
    private var txShifted = false
    def txencode(str: String) : Seq[Int] =
        {
        val buf = scala.collection.mutable.ListBuffer[Int]()
        for (c <- str)
            {
            if (c == ' ')
                buf += Baudot.BAUD_SPACE
            else if (c == '\n')
                buf += Baudot.BAUD_LF
            else if (c == '\r')
                buf += Baudot.BAUD_CR
            else
                {
                val uc = c.toUpper
                var code = Baudot.baudLtrsToCode.get(uc)
                if (code.isDefined)
                    {
                    if (txShifted)
                        {
                        txShifted = false
                        buf += Baudot.BAUD_LTRS
                        }
                    buf += code.get
                    }
                else
                    {
                    code = Baudot.baudFigsToCode.get(uc)
                    if (code.isDefined)
                        {
                        if (!txShifted)
                            {
                            txShifted = true
                            buf += Baudot.BAUD_FIGS
                            }
                        buf += code.get
                        }
                    }
                }
            }
        buf.toSeq
        }
    
    def txnext : Seq[Int] =
        {
        //val str = "the quick brown fox 1a2b3c4d"
        val str = par.gettext
        val codes = txencode(str)
        codes
        }
    
    
    private val desiredOutput = 4096


    /**
     * Overridded from Mode.  This method is called by
     * the audio interface when it needs a fresh buffer
     * of sampled audio data at its sample rate.  If the
     * mode has no current data, then it should send padding
     * in the form of what is considered to be an "idle" signal
     */                             
    override def transmit : Option[Array[Complex]] =
        {
        None
        }



}
