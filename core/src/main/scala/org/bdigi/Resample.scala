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


//Callbacks for java clients
trait FirResamplerOutput
{
    def apply(v: Double)
}

trait FirResamplerComplexOutput
{
    def apply(v: Complex)
}

/**
 * Let's use some of the tricks for a low-pass filter, but tailor it explicitly
 * This is a polyphase resampler, either for interpolating or decimating.
 */
class FirResampler(decimation: Int)
{
    private val size     = 3
    private val minus1   = size-1
    
    /**
     * Quick and easy low pass coefficients
     * Gave this class its own implementation for portability
     */
    private def lowPassCoeffs(decimation: Int, size: Int) : Array[Double] =
        {
        val twopi = 2.0 * math.Pi
        val omega = twopi / decimation
        val bottom = -0.5 * size
        val xs = Array.tabulate(size)( idx =>
            {
            //FIR coefficient
            val i = bottom + idx
            val fir = if (i == 0)
                omega / math.Pi 
            else 
                math.sin(omega * i) / (math.Pi * i)
            //Hamming window
            val window = 0.54 - 0.46 * math.cos(twopi * idx / (size-1))
            fir * window
            })
        xs
        }
   
    private val coeffs = lowPassCoeffs(decimation, decimation*size)

    private val polys = Array.tabulate(decimation, size) ( (p,i) => { val v = coeffs(i*decimation + p) ; /*println(p,i,v) ;*/ v })

    private var phase = 0
    private var sum   = 0.0
    private var rsum  = 0.0
    private var isum  = 0.0

    private val delayLine  = Array.fill(size)(0.0)
    private val delayLineX = Array.fill(size)(Complex(0.0))
    private var delayIndex = 0
    

    def decimate(sample: Complex)(f: Complex=>Unit) =
        {
        delayIndex = (delayIndex + minus1) % size
        delayLineX(delayIndex) = sample
        val poly = polys(phase)
        var idx = delayIndex
        for (coeff <- poly)
            {
            val v = delayLineX(idx)
            idx = (idx + 1) % size
            rsum += coeff * v.r           
            isum += coeff * v.i          
            }
        phase += 1
        if (phase >= decimation)
            {
            phase = 0
            f(Complex(rsum, isum))
            rsum = 0.0
            isum = 0.0
            }
        }
        
    /**
     * Generic decimator method
     */
    def decimate(sample: Double)(f: Double => Unit) =
        {
        delayIndex = (delayIndex + minus1) % size
        delayLine(delayIndex) = sample
        val poly = polys(phase)
        var idx = delayIndex
        for (coeff <- poly)
            {
            val v = delayLine(idx)
            idx = (idx + 1) % size
            sum += coeff * v            
            }
        phase += 1
        if (phase >= decimation)
            {
            phase = 0
            f(sum)
            sum = 0.0
            }
        }
        
    def interpolate(sample: Complex)(f: Complex=>Unit) =
        {
        delayIndex = (delayIndex + minus1) % size
        delayLineX(delayIndex) = sample
        for (poly <- polys)
            {
            var idx = delayIndex
            var rsum = 0.0
            var isum = 0.0
            for (coeff <- poly)
                {
                val v = delayLineX(idx)
                rsum += v.r * coeff
                isum += v.i * coeff
                idx = (idx + 1) % size
                }
            f(Complex(rsum, isum)*decimation)
            }
        }
        
    def interpolate(sample: Double)(f: Double => Unit) =
        {
        delayIndex = (delayIndex + minus1) % size
        delayLine(delayIndex) = sample
        for (poly <- polys)
            {
            var idx = delayIndex
            var sum = 0.0
            for (coeff <- poly)
                {
                val v = delayLine(idx)
                sum += v * coeff
                idx = (idx + 1) % size
                }
            f(sum*decimation)
            }
        }


}//Resampler


/**
 * A highly experimental resampler hardcoded with a decimation of 6 and 3 coefficients
 * per phase.  Probably won't work!  :)
 */
class Resampler6
{
    private val c00 =  1.0395124886718336E-18
    private val c01 =  1.0618775124613944E-17
    private val c02 =  8.652446849771314E-18
    private val c10 =  0.003827007298326752
    private val c11 =  0.12833556742764804
    private val c12 = -0.03428966321041717
    private val c20 =  0.007878328379302304
    private val c21 =  0.2735053406178378
    private val c22 = -0.018467332327256183
    private val c30 = -4.3524434203537546E-18
    private val c31 =  0.3307225419515316
    private val c32 = -2.5995075560632145E-18
    private val c40 = -0.02743173056833376
    private val c41 =  0.2566711348552961
    private val c42 =  0.004373722626659141
    private val c50 = -0.04589020349794127
    private val c51 =  0.1126381380499992
    private val c52 =  0.0027566444771089625

    trait Phase
    {
        def apply(v: Double)(f: Double => Unit)
        var next : Phase = this
    }

    private var d0 = 0.0
    private var d1 = 0.0
    private var d2 = 0.0
    private var sum = 0.0

    private val dec0 = new Phase {
        def apply(v: Double)(f: Double => Unit) =        
            sum = d0 * c00 + d1 * c01 + d2 * c02
    }
    private val dec1 = new Phase {
        def apply(v: Double)(f: Double => Unit) =
            sum += d0 * c10 + d1 * c11 + d2 * c12
    }
    private val dec2 = new Phase {
        def apply(v: Double)(f: Double => Unit) =
            sum += d0 * c20 + d1 * c21 + d2 * c22
    }
    private val dec3 = new Phase {
        def apply(v: Double)(f: Double => Unit) =
            sum += d0 * c30 + d1 * c31 + d2 * c32
    }
    private val dec4 = new Phase {
        def apply(v: Double)(f: Double => Unit) =
            sum += d0 * c40 + d1 * c41 + d2 * c42
    }
    private val dec5 = new Phase {
        def apply(v: Double)(f: Double => Unit) =
            { sum += d0 * c50 + d1 * c51 + d2 * c52 ; f(sum) }
    }

    dec0.next = dec1
    dec1.next = dec2
    dec2.next = dec3
    dec3.next = dec4
    dec4.next = dec5
    dec5.next = dec0

    private var dec : Phase = dec0


    def decimate(v: Double)(f: Double => Unit) =
    {
        d2 = d1 ; d1 = d0 ; d0 = v
        dec(v)(f)
        dec = dec.next
    }


    def interpolate(v: Double)(f: Double => Unit) =
    {
        d2 = d1 ; d1 = d0 ; d0 = v
        f(d0 * c00 + d1 * c01 + d2 * c02)
        f(d0 * c10 + d1 * c11 + d2 * c12)
        f(d0 * c20 + d1 * c21 + d2 * c22)
        f(d0 * c30 + d1 * c31 + d2 * c32)
        f(d0 * c40 + d1 * c41 + d2 * c42)
        f(d0 * c50 + d1 * c51 + d2 * c52)
    }


}


 


 
/**
 * Let's use some of the tricks for a low-pass filter, but tailor it explicitly
 */
class Decimator(size: Int, inRate: Double, outRate: Double)
{
    val lpf  = Fir.lowPass(size, inRate, outRate, Window.Hamming)

    var acc = -inRate

    def update(sample: Complex)(f: Complex=>Unit) =
        {
        val v = lpf.update(sample)
        acc += outRate
        if (acc > 0)
            {
            acc -= inRate
            f(v)
            }
        }
        
    def update(sample: Double)(f: Double => Unit) =
        {
        val v = lpf.update(sample)
        acc += outRate
        if (acc > 0)
            {
            acc -= inRate
            f(v)
            }
        }
        
}

