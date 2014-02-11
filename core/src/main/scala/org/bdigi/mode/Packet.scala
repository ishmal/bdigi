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


case class PacketAddr(call: String,  ssid:Int)
{

    lazy val encoded : Array[Int] =
        {
        val add = Array.tabulate(7)(i=>
             {
             if (i < call.size)
                 ((call(i).toInt) << 1)
             else if (i==6)
                 (0x60 | (ssid << 1))
             else
                 0x40   // shifted space
             })
        add
        }

    override def toString =
        {
        if (ssid >= 0) { call + "-" + ssid } else call
        }
  
}

class CrcCCITT
{
	private val crcTab = Array(
        0x0000, 0x1189, 0x2312, 0x329b, 0x4624, 0x57ad, 0x6536, 0x74bf,
        0x8c48, 0x9dc1, 0xaf5a, 0xbed3, 0xca6c, 0xdbe5, 0xe97e, 0xf8f7,
        0x1081, 0x0108, 0x3393, 0x221a, 0x56a5, 0x472c, 0x75b7, 0x643e,
        0x9cc9, 0x8d40, 0xbfdb, 0xae52, 0xdaed, 0xcb64, 0xf9ff, 0xe876,
        0x2102, 0x308b, 0x0210, 0x1399, 0x6726, 0x76af, 0x4434, 0x55bd,
        0xad4a, 0xbcc3, 0x8e58, 0x9fd1, 0xeb6e, 0xfae7, 0xc87c, 0xd9f5,
        0x3183, 0x200a, 0x1291, 0x0318, 0x77a7, 0x662e, 0x54b5, 0x453c,
        0xbdcb, 0xac42, 0x9ed9, 0x8f50, 0xfbef, 0xea66, 0xd8fd, 0xc974,
        0x4204, 0x538d, 0x6116, 0x709f, 0x0420, 0x15a9, 0x2732, 0x36bb,
        0xce4c, 0xdfc5, 0xed5e, 0xfcd7, 0x8868, 0x99e1, 0xab7a, 0xbaf3,
        0x5285, 0x430c, 0x7197, 0x601e, 0x14a1, 0x0528, 0x37b3, 0x263a,
        0xdecd, 0xcf44, 0xfddf, 0xec56, 0x98e9, 0x8960, 0xbbfb, 0xaa72,
        0x6306, 0x728f, 0x4014, 0x519d, 0x2522, 0x34ab, 0x0630, 0x17b9,
        0xef4e, 0xfec7, 0xcc5c, 0xddd5, 0xa96a, 0xb8e3, 0x8a78, 0x9bf1,
        0x7387, 0x620e, 0x5095, 0x411c, 0x35a3, 0x242a, 0x16b1, 0x0738,
        0xffcf, 0xee46, 0xdcdd, 0xcd54, 0xb9eb, 0xa862, 0x9af9, 0x8b70,
        0x8408, 0x9581, 0xa71a, 0xb693, 0xc22c, 0xd3a5, 0xe13e, 0xf0b7,
        0x0840, 0x19c9, 0x2b52, 0x3adb, 0x4e64, 0x5fed, 0x6d76, 0x7cff,
        0x9489, 0x8500, 0xb79b, 0xa612, 0xd2ad, 0xc324, 0xf1bf, 0xe036,
        0x18c1, 0x0948, 0x3bd3, 0x2a5a, 0x5ee5, 0x4f6c, 0x7df7, 0x6c7e,
        0xa50a, 0xb483, 0x8618, 0x9791, 0xe32e, 0xf2a7, 0xc03c, 0xd1b5,
        0x2942, 0x38cb, 0x0a50, 0x1bd9, 0x6f66, 0x7eef, 0x4c74, 0x5dfd,
        0xb58b, 0xa402, 0x9699, 0x8710, 0xf3af, 0xe226, 0xd0bd, 0xc134,
        0x39c3, 0x284a, 0x1ad1, 0x0b58, 0x7fe7, 0x6e6e, 0x5cf5, 0x4d7c,
        0xc60c, 0xd785, 0xe51e, 0xf497, 0x8028, 0x91a1, 0xa33a, 0xb2b3,
        0x4a44, 0x5bcd, 0x6956, 0x78df, 0x0c60, 0x1de9, 0x2f72, 0x3efb,
        0xd68d, 0xc704, 0xf59f, 0xe416, 0x90a9, 0x8120, 0xb3bb, 0xa232,
        0x5ac5, 0x4b4c, 0x79d7, 0x685e, 0x1ce1, 0x0d68, 0x3ff3, 0x2e7a,
        0xe70e, 0xf687, 0xc41c, 0xd595, 0xa12a, 0xb0a3, 0x8238, 0x93b1,
        0x6b46, 0x7acf, 0x4854, 0x59dd, 0x2d62, 0x3ceb, 0x0e70, 0x1ff9,
        0xf78f, 0xe606, 0xd49d, 0xc514, 0xb1ab, 0xa022, 0x92b9, 0x8330,
        0x7bc7, 0x6a4e, 0x58d5, 0x495c, 0x3de3, 0x2c6a, 0x1ef1, 0x0f78
        )

	private var crc = 0xffff
	
	def update(b: Int) =
	    crc = (crc >> 8) ^ crcTab((crc ^ b) & 0xff)
	    
	def value =
	    crc
	        
    def reset = 
        crc = 0xffff
}


/*
object CrcTest
{
    def test =
        {
        //val arr = "123456789".getBytes
        val arr = "the quick brown fox".getBytes
        val crc = new CrcCCITT
        for (b <- arr) crc.update(b.toInt & 255)
        val v = crc.value
        println("val: %04x".format(v))
        }
    
    
    
    def main(argv: Array[String]) : Unit =
        test
}
*/



case class Packet(
    val dest  : PacketAddr, 
    val src   : PacketAddr,
    val rpts  : Seq[PacketAddr], 
    val ctrl  : Int, 
    val pid   : Int,
    val info  : Array[Byte]
    )
{

    def toOctets : Array[Int] =
        {
        val buf = scala.collection.mutable.ListBuffer[Int]()
        buf += 0x7e // flag
        buf ++= dest.encoded
        buf ++= src.encoded
        for (rpt <- rpts)
            buf ++= rpt.encoded
        buf += ctrl
        buf += pid
        val crc = new CrcCCITT
        for (i <- buf)
            crc.update(i)
        val crcv = crc.value
        val fcslo = (crcv & 0xff) ^ 0xff
        val fcshi = (crcv >>   8) ^ 0xff
        buf += fcslo
        buf += fcshi
        buf += 0x7e // flag
        buf.toArray
        }   
        
    override def toString : String =
        {
        val infos = new String(info)
        "%s -> %s : %s".format(src.toString, dest.toString, infos) 
        } 
}


object Packet
{
    val PID_X25           = 0x01  // ISO 8208/CCITT X.25 PLP
    val PID_TCPIP_COMP    = 0x06  // Compressed TCP/IP packet. Van Jacobson (RFC 1144)
    val PID_TCPIP_UNCOMP  = 0x07  // Uncompressed TCP/IP packet. Van Jacobson (RFC 1144)
    val PID_FRAG          = 0x08  // Segmentation fragment
    val PID_AX25_FLAG1    = 0x10  // AX.25 layer 3 implemented.
    val PID_AX25_FLAG2    = 0x20  // AX.25 layer 3 implemented.
    val PID_AX25_MASK     = 0x30  // AX.25 layer 3 implemented.
    val PID_TEXNET        = 0xc3  // TEXNET datagram protocol
    val PID_LQP           = 0xc4  // Link Quality Protocol
    val PID_APPLETALK     = 0xca  // Appletalk
    val PID_APPLETALK_ARP = 0xcb  // Appletalk ARP
    val PID_ARPA_IP       = 0xcc  // ARPA Internet Protocol
    val PID_ARPA_ARP      = 0xcd  // ARPA Address Resolution
    val PID_FLEXNET       = 0xce  // FlexNet
    val PID_NETROM        = 0xcf  // NET/ROM
    val PID_NO_3          = 0xf0  // No layer 3 protocol implemented.
    val PID_ESCAPE        = 0xff  // Escape character. Next octet contains more Level 3 protocol information.
    
    /**
     * Frame identifiers
     */
    val FID_NONE     =  0  // Not an ID
    val FID_C        =  1  // Layer 2 Connect Request
    val FID_SABM     =  2  // Layer 2 Connect Request
    val FID_D        =  3  // Layer 2 Disconnect Request
    val FID_DISC     =  4  // Layer 2 Disconnect Request
    val FID_I        =  5  // Information frame
    val FID_RR       =  6  // Receive Ready. System Ready To Receive
    val FID_RNR      =  7  // Receive Not Ready. TNC Buffer Full
    val FID_NR       =  8  // Receive Not Ready. TNC Buffer Full
    val FID_RJ       =  9  // Reject Frame. Out of Sequence or Duplicate
    val FID_REJ      = 10  // Reject Frame. Out of Sequence or Duplicate
    val FID_FRMR     = 11  // Frame Reject. Fatal Error
    val FID_UI       = 12  // Unnumbered Information Frame. "Unproto"
    val FID_DM       = 13  // Disconnect Mode. System Busy or Disconnected.
    
    
    val IFRAME = 0
    val SFRAME = 1
    val UFRAME = 2
    
    private def getInt(s: String) : Int =
        {
        try
            {
            s.trim.toInt
            }
        catch
            {
            case e: Exception =>
                0
            }
        }
    
    private def getAddr(arr: Array[Int], offset:Int) : PacketAddr =
        {
        var buf = new StringBuilder
        for (i <- offset until offset + 7)
            buf.append( (arr(i) >> 1).toChar)
        val str = buf.toString
        //println("str:" + buf)
        val pair = str.split(" ")
        val call = if (pair.size > 0) pair(0) else ""
        val ssid = if (pair.size > 1) getInt(pair(1)) else -1
        new PacketAddr(call, ssid)
        }


    def apply(data : Array[Int]) : Packet =
        {
        var pos = 0
        val dest = getAddr(data, pos)
        pos += 7
        val src  = getAddr(data, pos)
        pos += 7
        val rpts = scala.collection.mutable.ListBuffer[PacketAddr]()
        if ((data(pos - 1) & 1) == 1)
            {
            rpts.append(getAddr(data, pos))
            pos += 7
            }
        if ((data(pos - 1) & 1) == 1)
            {
            rpts.append(getAddr(data, pos))
            pos += 7
            }
        val ctrl = data(pos)
        pos += 1
        
        val typ = if ((ctrl & 1) == 0) IFRAME else if ((ctrl & 2) == 0) SFRAME else UFRAME
        
        val pid = if (typ == IFRAME) data(pos) else 0
        if (typ == IFRAME) pos += 1
        
        val info = Array.tabulate(data.size - pos - 2)
            { i => data(i + pos).toByte }
            
        
        val pack = new Packet(dest, src, rpts.toSeq, 0, 0, info)
        pack
        }
    
}







/**
 * Mode for AX-25 packet communications.
 *  
 * @see http://www.tapr.org/pub_ax25.html
 */    
class PacketMode(par: App) extends Mode(par, 2400.0)
{
    override val name = "packet"
    override val tooltip = "AX.25 and APRS"
    
    private val rates = List(
         ( "300",  300.0),
         ("1200", 1200.0)
    )

    val shifts = List(
        ( "200",  200.0 ),
        ("1000", 1000.0 )
    )
    override val properties = new PropertyGroup(name,
        new RadioProperty("rate", "Rate", rates.map(_._1), "baud rate") (idx => rate = rates(idx)._2 ),
        new RadioProperty("shift", "Shift", shifts.map(_._1), "Spacing in hertz between mark and space", 1) ( idx => shift = shifts(idx)._2 )
    )
        
    private var shiftVal = 200.0
    
    def shift = shiftVal
    
    def shift_=(v: Double) =
        {
        shiftVal = v
        }
    
    override def rate_=(v: Double) =
        {
        super.rate = v
        adjust
        }

    rate      =  300.0
    shift     =  200.0
    
    override def bandwidth =
        shift
    
    var spaceFreq = Complex(twopi * (-shift * 0.5) / sampleRate)
    var markFreq  = Complex(twopi * ( shift * 0.5) / sampleRate)
    
    var sf = Fir.bandPass(13, -0.75 * shift, -0.25 * shift, sampleRate)
    var mf = Fir.bandPass(13,  0.25 * shift,  0.75 * shift, sampleRate)
    //var dataFilter = Iir2.lowPass(rate, sampleRate)
    var dataFilter = Fir.boxcar(samplesPerSymbol.toInt)
    var txlpf = Fir.lowPass(31,  shift * 0.5, sampleRate)
    
    var avgFilter = Iir2.lowPass(rate / 100, sampleRate)


    def adjust =
        {
        sf = Fir.bandPass(13, -0.75 * shift, -0.25 * shift, sampleRate)
        mf = Fir.bandPass(13,  0.25 * shift,  0.75 * shift, sampleRate)
        spaceFreq = Complex(twopi * (-shift * 0.5) / sampleRate)
        markFreq  = Complex(twopi * ( shift * 0.5) / sampleRate)
        //dataFilter = Iir2.lowPass(rate, sampleRate)
        dataFilter = Fir.boxcar(samplesPerSymbol.toInt)
        txlpf = Fir.lowPass(31,  shift * 0.5, sampleRate)
        }
        

    
    val loHys = -2.0
    val hiHys =  2.0

    var sym     = false 
    var lastSym = false   
    var samplesSinceTransition = 0

    var lastVal = Complex(0.0)
    
    /**
     * Basic receive function for all modes
     */         
    override def update(isample: Complex) : Double =
        {
        val space  = sf.update(isample)
        val mark   = mf.update(isample)
        val sample = space + mark
        val prod   = sample * lastVal.conj
        lastVal    = sample
        val demod  = prod.arg
        val comp   = math.signum(demod) * 10.0
        val sig    = dataFilter.update(comp)
        //trace("sig:" + sig + "  comp:" + comp)

        par.updateScope(sig, 0)

        //trace("sig:" + sig)
        if (sig > hiHys)
            {
            sym = true
            }
        else if (sig < loHys)
            {
            sym = false
            }

		if (sym != lastSym)
			samplesSinceTransition = 0
		else
			samplesSinceTransition += 1

		lastSym = sym

        process(sym)
        
        sig
		}
 
    
    trait RxState
    //the initial state
    case object RxStart extends RxState
    //after the first flag, wait until no more flags
    case object RxTxd   extends RxState
    //after the flag.  all octets until another flag
    case object RxData  extends RxState
    //Test whether we have a flag or a stuffed bit
    case object RxFlag1  extends RxState
    //It was a flag.  grab the last bit
    case object RxFlag2  extends RxState
    
    var state : RxState = RxStart
   
    val FLAG = 0x7e   // 01111110 , the start/stop flag
    
    var bitcount = 0
    var octet    = 0
    var ones     = 0

    
    var bufPtr = 0
    val rxbuf = Array.ofDim[Int](4096)
    
    var lastBit = false

    /**
     * Attempt to decode a packet.  It will be in NRZI form, so when
     * we sample at mid-pulse (period == halflen) we need to sense then
     * if the bit has flipped or not.  Do -not- check this for every sample.
     * the packet will be in the form:
     * 01111110 76543210 76543210 76543210 01234567 01234567 01111110
     *   flag    octet     octet   octet    fcs_hi   fcs_lo    flag
     */
    def process(inBit: Boolean) =
        {
        val symbollen = samplesPerSymbol.toInt
        val halflen   = symbollen >> 1
            
        val period = samplesSinceTransition % symbollen
        if (period == halflen)
            {
            octet = (octet >> 1) & 255
            val bit = (inBit == lastBit)
            lastBit = inBit
            if (bit) 
                { ones += 1 ; octet |= 128 }
            else
                ones = 0

            state match
                {
                case RxStart => 
                    //trace("RxStart")
                    //trace("st octet: %02x".format(octet))
                    if (octet == FLAG)
                        {
                        state    = RxTxd
                        bitcount = 0
                        }
                 case RxTxd => 
                    //trace("RxTxd")
                    bitcount += 1
                    if (bitcount >= 8)
                        {
                        //trace("txd octet: %02x".format(octet))
                        bitcount = 0
                        if (octet != FLAG)
                            {
                            state    = RxData
                            rxbuf(0) = octet & 255
                            bufPtr   = 1
                            }
                        }
                case RxData => 
                    //trace("RxData")
                    if (ones == 5) // 111110nn, next bit will determine
                        {
                        state = RxFlag1
                        }
                    else
                        {
                        bitcount += 1
                        if (bitcount >= 8)
                            {
                            bitcount = 0
                            if (bufPtr >= rxbuf.size)
                                {
                                //trace("drop")
                                state = RxStart
                                }
                            else
                                {
                                rxbuf(bufPtr) = octet & 255
                                bufPtr += 1
                                }
                            }
                        }
                case RxFlag1 =>
                    //trace("RxFlag")
                    if (bit) //was really a 6th bit. 
                        {
                        state = RxFlag2
                        }
                    else //was a zero.  drop it and continue
                        {
                        octet = octet << 1
                        state = RxData
                        }
                case RxFlag2 =>
                    //we simply wanted that last bit
                    processPacket(rxbuf, bufPtr)
                    for (i <- 0 until rxbuf.size)
                        rxbuf(i) = 0
                    state = RxStart                   
                }
            }
        }
    
    
    
    val crc = new CrcCCITT
    
    def intToStr(ibytes: Array[Int], offset: Int, len: Int) : String =
        {
        val bytes = Array.tabulate(len) { i=> ibytes(i+offset).toChar }
        new String(bytes)
        }        
    
    def processPacket(data: Array[Int], len: Int) : Boolean =
        {
        //println("raw:" + len)
        if (len < 14)
            return true
        val str = intToStr(data, 14, len-2)
        //trace("txt: " + str)
        crc.reset
        for (i <- 0 until len)
            crc.update(data(i))
        val v = crc.value
        //trace("crc: %04x".format(v))
        if (v == 0xf0b8)
            {
            val p = Packet(data)
            par.puttext(p.toString + "\n")
            }
        false
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
        val symbollen = samplesPerSymbol.toInt
        val buf = scala.collection.mutable.ListBuffer[Complex]()
        val codes = txnext
        for (code <- codes)
            {
            for (i <- 0 until symbollen) buf += spaceFreq
            var mask = 1 
            for (i <- 0 until 5)
                {
                val bit = (code & mask) == 0
                val f = if (bit) spaceFreq else markFreq
                for (j <- 0 until symbollen) buf += f
                mask <<= 1
                }
            for (i <- 0 until symbollen) buf += spaceFreq
            }
        
        val pad = desiredOutput - buf.size
        for (i <- 0 until pad)
            buf += spaceFreq
        //val res = buf.toArray.map(txFilter.update)
        None
        }



}


