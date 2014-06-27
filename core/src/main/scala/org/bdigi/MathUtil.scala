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

import org.apache.commons.math3.util.FastMath

case class Complex(r: Double = 0.0, i: Double = 0.0)
{
    lazy val mag  = r * r + i * i

    lazy val abs  = FastMath.sqrt(mag)

    lazy val norm = if (abs==0.0) this else Complex(r/abs, i/abs)
    
    lazy val arg  = FastMath.atan2(i, r)
    
    lazy val conj = Complex(r, -i)
    
    def +(other: Complex) = Complex(r + other.r, i + other.i)

    def -(other: Complex) = Complex(r - other.r, i - other.i)

    def %(other: Complex) = Complex(r % other.r, i % other.i)

    def *(other: Complex) = Complex(r * other.r - i * other.i, i * other.r + r * other.i)

    def *(scale: Double) = Complex(r * scale, i * scale)

    //see why you should avoid this? Use * instead.
    def /(other: Complex) = 
        {
        val div = r * r + other.r * other.r
        val ar = r * other.r + i * other.i
        val ai = i * other.r - r * other.i
        Complex(ar/div, ai/div)
        }

    def /(scale: Double) = Complex(r / scale, i / scale)
    
    def equals(other: Complex, epsilon: Double) : Boolean =
        {
        math.abs(r - other.r) < epsilon && FastMath.abs(i - other.i) < epsilon
        } 
}


object Complex
{
    def average(arr: Seq[Complex]) : Seq[Complex] =
        {
        var total = 0.0
        arr.foreach(c=> total += c.abs)
        val scale = Complex(1.0 / scala.math.sqrt(total/arr.size))
        arr.map(_ * scale)        
        }

    def normalize(arr: Seq[Complex], scale: Double = 1.0) : Seq[Complex] =
        {
        val maxc = arr.foldLeft(0.0) (_ max _.abs)
        val normScale = scale / maxc
        arr.map(c => Complex(c.r * normScale, c.i * normScale))     
        }

}


/**
 *  Place to put various math utilities
 */
object MathUtil
{
	/* fixed point multiply and scale */
	def mul(a: Short, b: Short)
	    {
		val c = (a.toInt * b.toInt) >> 14
		((c >> 1) + (c & 0x01)).toShort
	    }

    val twopi = math.Pi * 2.0
    private val tabMask = 0xffff;
    private val tabOmega = twopi / 65536.0
    private val tabRate = 1.0 / tabOmega
    private val cossinTab = Array.tabulate(65536)(i=>
         (math.cos(tabOmega*(0.5+i)), math.sin(tabOmega*(0.5+i)) ))
         
    
    /**
     * return an estimated cosine, sine tuple for the given angle from 0-2.0*Pi
     * @param angle the angle used for lookup
     * @return the cosine, sine tuple
     */
    def cossin(angle: Double) : (Double, Double) =
        {
        cossinTab((angle * tabRate).toInt & tabMask)
        }

    
    /**
     * A rough approximation for math.log() when accuracy is not
     * required.  Good for log-like behavior, such as for drawing things.
     * Should be a few times faster.
     * @param v the value for which to estimate the log
     * @return the estimated log
     *
     */              
    def log(v: Double) : Double =
        {
        6.0 * ( v - 1.0 ) / ( v + 1.0 + 4.0 * math.sqrt(v) )
        }
        
    /**
     * Faster but less accurate
     * @see http://code.google.com/p/fastapprox
     * In tests, almost 3 times faster than above.
     * @param v the value for which to estimate the log
     * @return the estimated log
     *
     */              
    def log2(v: Float) : Float =
        {
        var x = java.lang.Float.floatToRawIntBits(v).toFloat
        return x * 8.2629582881927490e-8f - 87.989971088f
        }
        
    /**
     * Same as above, but multiplied by 10
     * @param v the value for which to estimate the log
     * @return the estimated log
     *
     */              
    def log2_1p(v: Float) : Float =
        {
        var x = java.lang.Float.floatToRawIntBits(v+1).toFloat
        return x * 8.2629582881927490e-8f - 87.989971088f
        }
        
    def log1p(v: Double) : Double =
        FastMath.log1p(v)
        
    /**
     * The sinc function.  Returns sin(2Pi) / 2Pi
     * @param x the value for which to look up the sinc() function
     * @return the sinc value for x
     */
    def sinc(x: Double) : Double =
        {
        val piX = math.Pi * x
        math.sin(piX) / piX
        }

    /**
     * Shift a set of sampled points by a given frequency
     * @param raw the set of data points to shift
     * @param freq the frequency by which to shift the points
     * @param Fs the sample frequency of the sampled points
     * @return a set of shifted complex data points
     */
    def shift(raw: Seq[Double], freq: Double, Fs: Double) : Seq[Complex] =
        {
        var angle = 0.0
        val delta = 2.0 * math.Pi * freq / Fs
        raw.map(v =>
            {
            val cpx = Complex(v * math.cos(angle), v * math.sin(angle) )
            angle += delta
            cpx
            })
        }

    /**
     * This is a crude, crowbar approach for getting a working ratio
     * for downsampling from a value and a given error.
     * Good enough for us!     
     * 0.0000001 is a good value for "err".  YMMV
     */                   
    def toFraction(v: Double, err: Double) : (Int, Int) =
        {
        if (err < 0.0 || v < err)
            throw new IllegalArgumentException("positive values only. was: " + v + ", " + err)
        var num = v.toInt //start off at v/1
        var denom = 1
        var keepGoing = true
        while (keepGoing)
            {
            val ratio = num.toDouble / denom.toDouble
            if (math.abs(v-ratio) < err)
                keepGoing = false
            else if (ratio < v)
                num += 1
            else
                denom += 1
            }
        (num, denom)
        }


}



class Decimal(val v: Int) extends AnyVal
{
    def +(other: Decimal) : Decimal =
        new Decimal(v + other.v)
    def -(other: Decimal) : Decimal =
        new Decimal(v - other.v)

    def *(other: Decimal) : Decimal= 
        {
        val lv = v.toLong * other.v
        new Decimal((lv>>16).toInt)
        }
        
    def toDouble : Double =
        v.toDouble * Decimal.from16
    
    override def toString : String =
        toDouble.toString
}

object Decimal
{
    val to16   = 65536.0
    val from16 = 1.0 / 65536.0
    
    def apply(v: Double) : Decimal =
        new Decimal(math.rint(v * to16).toInt)

    def apply(v: Int) : Decimal =
        new Decimal(v)
}





