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
 * This is a receive-only Mode for decoding Navtex transmissions
 * using the SITOR-B transport.
 */
class Navtex(par: App) extends Mode(par, 1000.0)
{

    override val name = "navtex"
    override val tooltip = "SITOR B weather bulletins on 490khz and 518khz"
    override val properties = new PropertyGroup(name,
    
        new BooleanProperty("inv", "Inv", "Switches mark and space for USB and LSB") ( b => inverted = b )
    
    )
    
    //set shift before rate, to get the filters right
    val shift = 170.0
    rate      = 100.0
    
    status("samplesPerSymbol: " + samplesPerSymbol)

    var inverted = false
 
    override def bandwidth =
        shift
        
    val sf = Fir.bandPass(13, -0.5*(shift+rate), -0.5*(shift-rate), sampleRate)
    val mf = Fir.bandPass(13,  0.5*(shift-rate),  0.5*(shift+rate), sampleRate)

    val dataFilter = Fir.boxcar(samplesPerSymbol.toInt)
 
    //only change the bit when it goes above or below these values
    val loHys = -5.0
    val hiHys =  5.0

    //the output bit
    var bit     = false
    //these two vars are used to mark the moment of the last zero-crossing
    var lastBit = false
    var samplesSinceTransition = 0
    
    //this is our single delay element used for FM demod
    var lastVal = Complex(0.0)
    
	/**
	 * Overridden from Mode for processing incoming
	 * Double-valued audio samples.  This code uses
	 * FM demodulation to sense the FSK shifts.  The
	 * current complex sample is mixed with the conjugate
	 * of the previous sample to get the instantaneous
	 * frequency shift of the baseband signal.
	 */
    override def update(isample:  Complex) : Double =
        {
        val space  = sf.update(isample)
        val mark   = mf.update(isample)
        val sample = space + mark
		val prod   = sample * lastVal.conj
		lastVal    = sample
		val demod  = prod.arg
		val comp   = math.signum(demod) * 10.0
		val sig    = dataFilter.update(comp)
		
		par.updateScope(sig, 0)

		if (sig > hiHys)
			bit = true
		else if (sig < loHys)
			bit = false
			
		if (bit != lastBit)
			samplesSinceTransition = 0
		else
			samplesSinceTransition += 1
		lastBit = bit

		process(bit)
		
		sig
        }

    

    trait RxState
    case object RxSync1  extends RxState
    case object RxSync2  extends RxState
    case object RxData   extends RxState
    
    var state : RxState = RxSync1
    var code      = 0
    var bitCount  = 0
    val symbollen = samplesPerSymbol.toInt
    val halflen   = symbollen / 2
    
    /**
     * Since there is no start or stop bit, we must sync ourselves.
     * But syncing is very simple.  We shift the bits through four 7-bit
     * shift registers.  When all four have valid characters, we consider
     * it to be synced.
     */
    var errs      = 0
    var sync1     = 0
    var sync2     = 0
    var sync3     = 0
    var sync4     = 0
    
    def shift7(bit: Boolean) =
        {
        var a = if (bit) 1 else 0
        var b = (sync1 >> 6) & 1
        sync1 = ((sync1 << 1) + a) & 0x7f
        a = b
        b = (sync2 >> 6) & 1
        sync2 = ((sync2 << 1) + a) & 0x7f
        a = b
        b = (sync3 >> 6) & 1
        sync3 = ((sync3 << 1) + a) & 0x7f
        a = b
        sync4 = ((sync4 << 1) + a) & 0x7f
        }
   
    def process(inbit: Boolean) =
        {
        val bit = inbit ^ inverted
        val period = samplesSinceTransition % symbollen

        state match
            {
            case RxSync1 =>
                //trace("RxSync1")
                state    = RxSync2
                code     = 0
                bitCount = 0
                errs     = 0
                sync1    = 0
                sync2    = 0
                sync3    = 0
                sync4    = 0
            case RxSync2 => 
                //trace("RxSync2")
                if (period == halflen)
                    {
                    shift7(bit)
                    //trace(sync1.toHexString + ", "+  sync2.toHexString + ", " +
                    //     sync3.toHexString + ", " + sync4.toHexString)
                    //trace("bit: " + bit)
                    if (isValid(sync1) && isValid(sync2) &&
                        isValid(sync3) && isValid(sync4))
                        {
                        processCode(sync1)
                        processCode(sync2)
                        processCode(sync3)
                        processCode(sync4)
                        state     = RxData
                        }
                    }
            case RxData => 
                //trace("RxData")
                if (period == halflen)
                    {
                    code =  ((code<<1) + (if (bit) 1 else 0)) & 0x7f
                    //trace("code: " + code)
                    bitCount += 1
                    if (bitCount >= 7)
                        {
                        if (processCode(code) != ResultFail) //we want Ok or Soft
                            {
                            //stay in RxData.  ready for next code
                            code      = 0
                            bitCount  = 0
                            }
                        else
                            {
                            code      = 0
                            bitCount  = 0
                            errs += 1
                            if (errs > 3)
                                {
                                state = RxSync1
                                //trace("return to sync")
                                }
                            }
                        }
                    }
            }//match
        }
    
    var shifted = false 
        
    var unshiftOnSpace = false
    
    def reverse(v: Int, len: Int) : Int =
        {
        var a = v
        var b = 0
        for (i <- 0 until len)
            {
            b = (b<<1) + (a&1)
            a >>= 1
            }
        b
        }

    trait Result
    case object ResultOk   extends Result
    case object ResultSoft extends Result
    case object ResultFail extends Result
    case object ResultEom  extends Result


    //Sitor-B is in either DX (data) or RX (repeat) mode
    var dxMode = true

    var q3 = 0
    var q2 = 0
    var q1 = 0
    
    def qadd(v: Int) =
        {
        q3 = q2
        q2 = q1
        q1 = v
        }
    
    def isValid(code: Int) : Boolean =
        Baudot.ccirIsValid(code)

    def processCode(code: Int) : Result =
        {
        //trace("code: " + code.toHexString + " mode: " + dxMode)
        var res : Result = ResultOk
        if (code == Baudot.CCIR_REPEAT)
            {
            qadd(code)
            shifted = false
            dxMode = false
            }
        else if (code == Baudot.CCIR_ALPHA)
            {
            shifted = false
            dxMode = true
            }
        else
            {
            if (dxMode)
                {
                if (!isValid(code))
                    res = ResultSoft
                qadd(code) //dont think.  just queue it
                dxMode = false //for next time
                }
            else //symbol
                {
                if (isValid(code))
                    {
                    processCode2(code)
                    }
                else
                    {
                    if (isValid(q3))
                        {
                        val c = processCode2(q3)
                        par.status("FEC replaced :" + c)
                        res = ResultSoft
                        }
                    else
                        {
                        processCode2(-1)
                        res = ResultFail
                        }
                    }
                dxMode = true // next time
                }//rxmode
            }//symbol
        res
        }

    var lastChar = '@'

    def processCode2(code: Int) : Char =
        {
        var res = '@'
        if (code == 0)
            {
            //shouldnt happen
            }
        else if (code < 0)
            {
            //par.puttext("_")
            res = '_'
            }
        else if (code == Baudot.CCIR_ALPHA || code == Baudot.CCIR_REPEAT)
            {
            //shouldnt be here
            }
        else if (code == Baudot.CCIR_LTRS)
            {
            shifted = false
            }
        else if (code == Baudot.CCIR_FIGS)
            {
            shifted = true
            }
        else if (code == Baudot.CCIR_SPACE)
            {
            par.puttext(" ")
            }
        else if (code == Baudot.CCIR_CR || code == Baudot.CCIR_LF)
            {
            par.puttext("\n")
            }
        else
            {
            val ch = Baudot.ccirCodeToSym.get(code)
            if (ch.isDefined)
                {
                val v = ch.get
                val c = if (shifted) v._2 else v._1
                if (c > 0)
                    par.puttext(c.toChar.toString)
                res = c.toChar
                }
            }
        lastChar = res
        res
        }
    


}
