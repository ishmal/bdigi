/**
 * Scala SDR tool
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2013 Bob Jamison
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


object Psk
{
    val bpskConst = Array(
        Complex( 1.0,  0.0),
        Complex(-1.0,  0.0)
        )
    val qpskConst = Array(
        Complex( 1.0,  0.0),
        Complex( 0.0,  1.0),
        Complex(-1.0,  0.0),
        Complex( 0.0, -1.0)
        )

    val constellations = Array(bpskConst, qpskConst)
}


/**
 * This contains the definitions of the bit patterns for the Varicode set 
 * of characters.  
 * 
 * A "from" and a "to" table are also provided.
 */
object Varicode
{
    val description = Array(
        "1010101011",  //  0  00  NUL Null character
        "1011011011",  //  1  01  SOH Start of Header
        "1011101101",  //  2  02  STX Start of Text 
        "1101110111",  //  3  03  ETX End of Text   
        "1011101011",  //  4  04  EOT End of Transmission
        "1101011111",  //  5  05  ENQ Enquiry       
        "1011101111",  //  6  06  ACK Acknowledgment
        "1011111101",  //  7  07  BEL Bell          
        "1011111111",  //  8  08  BS  Backspace     
        "11101111",    //  9  09  HT  Horizontal Tab
        "11101",       // 10  0A  LF  Line feed     
        "1101101111",  // 11  0B  VT  Vertical Tab  
        "1011011101",  // 12  0C  FF  Form feed     
        "11111",       // 13  0D  CR  Carriage return
        "1101110101",  // 14  0E  SO  Shift Out     
        "1110101011",  // 15  0F  SI  Shift In      
        "1011110111",  // 16  10  DLE Data Link Escape
        "1011110101",  // 17  11  DC1 Device Control 1 (XON)
        "1110101101",  // 18  12  DC2 Device Control 2
        "1110101111",  // 19  13  DC3 Device Control 3 (XOFF)
        "1101011011",  // 20  14  DC4 Device Control 4
        "1101101011",  // 21  15  NAK Negative Acknowledgement
        "1101101101",  // 22  16  SYN Synchronous Idle
        "1101010111",  // 23  17  ETB End of Trans. Block
        "1101111011",  // 24  18  CAN Cancel        
        "1101111101",  // 25  19  EM  End of Medium 
        "1110110111",  // 26  1A  SUB Substitute    
        "1101010101",  // 27  1B  ESC Escape        
        "1101011101",  // 28  1C  FS  File Separator
        "1110111011",  // 29  1D  GS  Group Separator
        "1011111011",  // 30  1E  RS  Record Separator
        "1101111111",  // 31  1F  US  Unit Separator
        "1",           // 32  20  SP                
        "111111111",   // 33  21  !                 
        "101011111",   // 34  22  "                 
        "111110101",   // 35  23  #                 
        "111011011",   // 36  24  $                 
        "1011010101",  // 37  25  %                 
        "1010111011",  // 38  26  &                 
        "101111111",   // 39  27  '                 
        "11111011",    // 40  28  (                 
        "11110111",    // 41  29  )                 
        "101101111",   // 42  2A  *                 
        "111011111",   // 43  2B  +                 
        "1110101",     // 44  2C  ",                 
        "110101",      // 45  2D  -                 
        "1010111",     // 46  2E  .                 
        "110101111",   // 47  2F  /                 
        "10110111",    // 48  30  0                 
        "10111101",    // 49  31  1",  //              
        "11101101",    // 50  32  2                 
        "11111111",    // 51  33  3                 
        "101110111",   // 52  34  4                 
        "101011011",   // 53  35  5                 
        "101101011",   // 54  36  6                 
        "110101101",   // 55  37  7                 
        "110101011",   // 56  38  8                 
        "110110111",   // 57  39  9                 
        "11110101",    // 58  3A  :                 
        "110111101",   // 59  3B  ;                 
        "111101101",   // 60  3C  <                 
        "1010101",     // 61  3D  =                 
        "111010111",   // 62  3E  >                 
        "1010101111",  // 63  3F  ?                 
        "1010111101",  // 64  40  @                 
        "1111101",     // 65  41  A                 
        "11101011",    // 66  42  B                 
        "10101101",    // 67  43  C                 
        "10110101",    // 68  44  D                 
        "1110111",     // 69  45  E                 
        "11011011",    // 70  46  F                 
        "11111101",    // 71  47  G                 
        "101010101",   // 72  48  H                 
        "1111111",     // 73  49  I                 
        "111111101",   // 74  4A  J                 
        "101111101",   // 75  4B  K                 
        "11010111",    // 76  4C  L                 
        "10111011",    // 77  4D  M                 
        "11011101",    // 78  4E  N                 
        "10101011",    // 79  4F  O                 
        "11010101",    // 80  50  P                 
        "111011101",   // 81  51  Q                 
        "10101111",    // 82  52  R                 
        "1101111",     // 83  53  S                 
        "1101101",     // 84  54  T                 
        "101010111",   // 85  55  U                 
        "110110101",   // 86  56  V                 
        "101011101",   // 87  57  W                 
        "101110101",   // 88  58  X                 
        "101111011",   // 89  59  Y                 
        "1010101101",  // 90  5A  Z                 
        "111110111",   // 91  5B  [                 
        "111101111",   // 92  5C  \                 
        "111111011",   // 93  5D  ]                 
        "1010111111",  // 94  5E  ^                 
        "101101101",   // 95  5F  _                 
        "1011011111",  // 96  60  `                 
        "1011",        // 97  61  a                 
        "1011111",     // 98  62  b                 
        "101111",      // 99  63  c                 
        "101101",      //100  64  d                 
        "11",          //101  65  e                 
        "111101",      //102  66  f                 
        "1011011",     //103  67  g                 
        "101011",      //104  68  h                 
        "1101",        //105  69  i                 
        "111101011",   //106  6A  j                 
        "10111111",    //107  6B  k                 
        "11011",       //108  6C  l                 
        "111011",      //109  6D  m                 
        "1111",        //110  6E  n                 
        "111",         //111  6F  o                 
        "111111",      //112  70  p                 
        "110111111",   //113  71  q                 
        "10101",       //114  72  r                 
        "10111",       //115  73  s                 
        "101",         //116  74  t                 
        "110111",      //117  75  u                 
        "1111011",     //118  76  v                 
        "1101011",     //119  77  w                 
        "11011111",    //120  78  x                 
        "1011101",     //121  79  y                 
        "111010101",   //122  7A  z                 
        "1010110111",  //123  7B  {                 
        "110111011",   //124  7C  |                 
        "1010110101",  //125  7D  }                 
        "1011010111",  //126  7E  ~                 
        "1110110101"   //127  7F  DEL  Delete     
        ) 
        
        
    def toString(i: Int) =
        java.lang.Integer.toString(i, 2)
        
    /**
     * this is a table of index->bit seqs.  Ex: 116('t') is Seq(true, false, true)
     */         
    val encodeTable : Array[Array[Boolean]] =
        description.map(s =>
            {
            s.getBytes.map(_ == '1')
            })
        
    val decodeTable =
        {
        val ints = description.map(s => java.lang.Integer.parseInt(s.trim, 2).toInt)
        ints.zipWithIndex.toMap
        }
        
    def printTables =
        {
        println("Encode Table ================= : " + encodeTable)
        for (i <- 0 until encodeTable.size)
            {
            println(""+ i + " : " + encodeTable(i).mkString(","))
            }
        println("Decode Table ================= : " + decodeTable)
        val sortedKeys = decodeTable.toList.map(_._1).sortWith(_ < _)
        for (key <- sortedKeys)
            {
            val asc = decodeTable(key)
            println(java.lang.Integer.toString(key,2) + " : "+ asc)
            }
            
        }
        
}



class EarlyLate(samplesPerSymbol: Double)
{
    val size    = samplesPerSymbol.toInt
    val half    = size / 2
    val buf     = Array.fill(size)(0.0)
    var bitclk  = 0.0

    def update(z: Complex)(f: (Complex) => Unit) =
        {
        val idx    = bitclk.toInt
        var sum    = 0.0
        var ampsum = 0.0
        var mag    = z.mag
        buf(idx)   = 0.8 * buf(idx) + 0.2 * mag
        
        for (i <- 0 until half) 
            {
            sum    += (buf(i) - buf(i+half))
            ampsum += (buf(i) + buf(i+half))
            }

        val err = if (ampsum == 0.0) 0.0 else sum / ampsum * 0.2
    
        bitclk += (1.0 - err)
        if (bitclk < 0) 
            bitclk += size
        else if (bitclk >= size) 
            {
            bitclk -= size
            f(z)
            }
        
        }
}



class MaximumLikelihood(constellation: Array[Complex])
{
    val size  = constellation.size
    val delay = Array.ofDim[Double](size)
    var ptr   = 0
    var sum   = 0.0

    def update(sample: Complex) : Complex =
        {
        val v = sample.mag
        sum = v - delay(ptr)
        delay(ptr) = v
        ptr = (ptr + 1) % size
            
        var argmax = -math.Pi
        var minval = 1000000.0
        val resolution = 100
        val delta = math.Pi * 2.0 / resolution
        var arg = 0.0
        for (iter <- 0 until resolution)
            {
            var total = 0.0
            var iptr = ptr
            for (i <- 0 until size)
                {
                val v = delay(iptr)
                iptr = (iptr + 1) % size
                for (point <- constellation)
                    {
                    val diff = v - arg
                    sum += diff * diff
                    }
                }
             if (sum < minval)
                 {
                 minval = sum
                 argmax = arg
                 }
             arg += delta
             }        
        Complex(v)
        }
}

object MaximumLikelihood
{
    def bpsk : MaximumLikelihood =
        new MaximumLikelihood(Array(Complex(1.0), Complex(-1.0)))

    def qpsk : MaximumLikelihood =
        new MaximumLikelihood(Array(Complex(1.0, 0.0), Complex(0.0, 1.0),
                 Complex(-1.0, 0.0), Complex(0.0, -1.0)))
}





class Psk31(par: App) extends Mode(par, 1000.0)
{
    //####################################################
    //# S E T T I N G S
    //####################################################
    rate       = 31.25
    
    override val name = "psk"
    override val tooltip = "Phase shift keying"
    
    private val rates = List(
         ("31", 31.25),
         ("63",  62.5),
         ("125", 125.0)
    )
    
    private val modes = List(
         ("BPSK", false),
         ("QPSK",  true)
    )
    
    
    override val properties = new PropertyGroup(name, 
    
        new RadioProperty("rate", "Rate", rates.map(_._1), "PSK bit rate") (idx => rate = rates(idx)._2 ),
        new RadioProperty("mode", "Mode", modes.map(_._1), "BPSK or QPSK") (idx => qpskMode = (idx == 1) )
    
    )
    
    
    trace("sampleRate: " + sampleRate + "  samplesPerSymbol: " + samplesPerSymbol)

    var costas  = new CostasLoop2(frequency, rate,  sampleRate)

    var dataFilter = Fir.raisedCosine(samplesPerSymbol.toInt * 4 + 1, 0.35, rate, sampleRate)
    var bpf = Fir.bandPass(13, -0.7*rate, 0.7*rate, sampleRate)
    var timer = new EarlyLate(samplesPerSymbol)
    
    def makeTxShape : Array[Double] =
        {
        Array.tabulate(samplesPerSymbol.toInt)(i =>
            {
            0.5 * math.cos(math.Pi * i / samplesPerSymbol) + 0.5
            })
        }
    
    var txShape = makeTxShape
    var txFilter = Fir.lowPass(31, frequency + rate*0.5, sampleRate)
    
    var useCostas = false
    
    override def rate_=(v: Double) =
        {
        super.rate = v
        //costas  = new CostasLoop2(frequency, rate,  sampleRate)
        txShape = makeTxShape
        bpf = Fir.bandPass(13, -0.7*rate, 0.7*rate, sampleRate)
        dataFilter = Fir.raisedCosine(samplesPerSymbol.toInt * 4 + 1, 0.35, rate, sampleRate)
        timer = new EarlyLate(samplesPerSymbol)
        }

    //####################################################
    //# D E M O D U L A T E
    //####################################################


    override def update(isample: Complex) : Double =
        {
        var sample = bpf.update(isample)
        val z = dataFilter.update(sample)
        //var zscope = z * 2.0;
        par.updateScope(z.r, z.i)
        timer.update(z)(processSymbol)
        z.r
        }
    
    
    /*
    var lastx = Complex(0.0)
 
    def update(sample: Complex) : Unit =
        {
        val x = sample
        val z = x * lastx
        lastx = x
        par.updateScope(z * 6.0)
        timer.update(z)(processSymbol)
        }
    */

    //####################################################
    //# D E C O D E    
    //####################################################

    val decoder = Viterbi.decoder(5, 0x17, 0x19)
    
    var qpskMode = false
    
        
    def angleDiff(a: Double, b: Double) : Double =
        {
        var diff = a-b
        while (diff > math.Pi)
            diff -= twopi
        while (diff < -math.Pi)
            diff += twopi
        //println("%f %f %f".format(a, b, diff))
        diff
        }

    private val diffScale = 255.0 / math.Pi
    /**
     * Return the scaled distance of the angle v from "from".
     * Returns a positive value 0..255  for
     * 0 radians to +- pi       
     */    
    def distance(v: Double, from: Double) : Int =
        {
        val diff = math.Pi - math.abs(math.abs(v-from) - math.Pi)
        (diff * diffScale).toInt
        }

    val halfpi = math.Pi * 0.5

    var code      = 0
    var lastv     = 0.0
    var count     = 0
    var lastBit   = false
    
    
    def processSymbol(v: Complex) =
        {
        if (qpskMode)
            {
            /**/
            val vn  = v.arg
            val dv  = angleDiff(vn,  lastv)
            val d00 = distance(dv, math.Pi)
            val d01 = distance(dv,  halfpi)
            val d10 = distance(dv, -halfpi)
            val d11 = distance(dv,     0.0)
            val bm = Array(d00, d01, d10, d11)
            //println("%6.3f %6.3f %6.3f  :  %3d %3d %3d %3d".format(lastv, vn, dv, d00, d01, d10, d11))
            val bits = decoder.decodeOne(bm) 
            for (bit <- bits)
                processBit(bit)
            lastv = vn
            /**/               
            }
        else //bpsk
            {
            /**/
            val vn  = v.arg
            val dv  = angleDiff(vn,  lastv)
            val d00 = distance(dv, math.Pi)
            val d11 = distance(dv,     0.0)
            //println("%6.3f %6.3f %6.3f  :  %3d %3d".format(lastv, vn, dv, d00, d11))
            val bit = d11 < d00
            lastv = vn
            /**/
            processBit(bit)
            }
        }


    def processBit(bit: Boolean) =
        {
        //println("bit: " + bit)
        if ((!bit) && (!lastBit))
            {
            code >>= 1   //remove trailing 0
            if (code != 0)
                {
                //println("code:" + Varicode.toString(code))
                val ascii = Varicode.decodeTable.get(code)
                if (ascii.isDefined)
                    {
                    val chr = ascii.get.toChar
                    if (chr == 10 || chr == 13)
                        par.puttext("\n")
                    else
                        par.puttext(chr.toString)
                    code = 0
                    }                        
                }
            code = 0
            }
        else
            {
            code <<= 1
            if (bit) code += 1
            }
        lastBit = bit        
        }
    
    //###########################################################
    //# T R A N S M I T
    //###########################################################

    val encoder = Viterbi.encoder(5, 0x17, 0x19)
    
    private val rotate = Array(
        Complex( 1.0,  0.0),  //  00 ->   0 deg
        Complex( 0.0,  1.0),  //  01 ->  90 
        Complex(-1.0,  0.0),  //  10 -> 180
        Complex( 0.0, -1.0)   //  11 -> 270
        )
    
    private var lastEncSym = Complex(1.0)
    // expect 0, 1, 2, 3
    private def txEnc(dibit: Int) : Complex =
        {
        val rot = if (qpskMode) rotate(dibit ^ 3) else rotate(dibit)
        //println("dibit: " + dibit + " rot: " + rot)
        val sym = lastEncSym * rot
        lastEncSym = sym
        sym  
        }

    private val desiredOutput = 100
    
    private def txEnc(str: String) : Array[Complex] =
        {
        val zero = if (qpskMode) 0 else 2
        val buf = scala.collection.mutable.ListBuffer[Complex]()
        for (c <- str)
            {
            val code = c.toInt
            if (code < 128)
                {
                val bits = Varicode.encodeTable(code)
                for (b <- bits)
                    {
                    if (qpskMode)
                        buf += txEnc(encoder.encode(b))
                    else
                        buf += txEnc(if (b) 0 else 2)
                    }
                buf += txEnc(zero)
                buf += txEnc(zero) 
                } 
            }

        val pad = desiredOutput - buf.size
        for (i <- 0 until pad)
            buf += txEnc(zero)

        buf.toArray
        }

    
    private def txNext : Array[Complex] =
        {
        txEnc(par.gettext)
        }

    private var txPrevSym = Complex(-1.0)

    override def transmit : Option[Array[Complex]] =
        {
        val symbollen = samplesPerSymbol.toInt
        val buf = scala.collection.mutable.ListBuffer[Complex]()
        val syms = txNext
        for (sym <- syms)
            {
            for (i <- 0 until symbollen)
                {
                val shapeA = txShape(i)
                val shapeB = 1.0 - shapeA
        
                val iq   = txPrevSym * shapeA + sym * shapeB
        
                buf += iq
                }
            
            txPrevSym = sym
            }
        
        val res = buf.toArray.map(txFilter.update)
        Some(res)
        }

}




class QuadCodec
{
    var lastenc = Complex(1.0)

    def encode(v: Int) : Complex =
        {
        val enc = if (v == 0)
            {
            Complex(-lastenc.r, -lastenc.i)  //opposite
            }
        else if (v == 1)  
            {
            Complex(-lastenc.i, lastenc.r)    //90 deg counterclockwise
            }
        else if (v == 2)
            {
            Complex(lastenc.i, -lastenc.r)  //90 deg clockwise
            }
        else
            {
            lastenc  //dont move
            }
        println("out: %d %s %s".format(v, lastenc, enc))
        lastenc = enc
        enc
        }
    
    def encode(arr: Seq[Int]) : Seq[Complex] =
        arr.map(encode)
            


    def angleDiff(a: Double, b: Double) : Double =
        {
        var diff = a-b
        if (diff > math.Pi)
            diff -= math.Pi
        else if (diff < -math.Pi)
            diff += math.Pi
        //println("%f %f %f".format(a, b, diff))
        diff
        }

    private val diffScale = 255.0 / math.Pi
    /**
     * Return the scaled distance of the angle v from "from".
     * Returns a positive value 0..255  for
     * 0 radians to +- pi       
     */    
    def distance(v: Double, from: Double) : Int =
        {
        val diff = math.Pi - math.abs(math.abs(v-from) - math.Pi)
        (diff * diffScale).toInt
        }

    val halfpi = math.Pi * 0.5

    var lastAngle = 0.0
    
    def decode(v: Complex) : Seq[Int] =
        {
        val angle = v.arg
        val dv  = angleDiff(angle, lastAngle)
        val d00 = distance(dv, math.Pi)
        val d01 = distance(dv,  halfpi)
        val d10 = distance(dv, -halfpi)
        val d11 = distance(dv,     0.0)
        val bm = Array(d00, d01, d10, d11)
        println("%6.3f %6.3f %6.3f  :  %3d %3d %3d %3d".format(lastAngle, angle, dv, d00, d01, d10, d11))
        lastAngle = angle
        bm
        }
    
    
    def toIndex(arr: Seq[Int]) : Int =
        {
        var idx = 0
        var min = Int.MaxValue
        for (i <- 0 until arr.size)
            {
            val v = arr(i)
            if (v < min)
                {
                min = v
                idx = i
                }
            }
        idx
        }
    
    
}


