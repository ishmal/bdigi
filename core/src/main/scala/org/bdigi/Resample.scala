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

    private val polys = Array.tabulate(decimation, size) ( (p,i) => coeffs(i*decimation + p) )

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

