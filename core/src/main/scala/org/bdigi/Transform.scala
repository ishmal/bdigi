/**
 * Scala SDR tool
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (c) 2014 Bob Jamison
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



trait ZTransform
{
    def X : Array[Complex]

    def powerSpectrum : Array[Double]
}



/**
 * Perform a Goertzel analysis for a single target frequency
 * of an incoming sample stream.  This is basically one bin of
 * a DFT.  For certain tasks (sensing a tone at a given frequency)
 * this can be far more efficient than analyzing a frequency range 
 * with an FFT.
 * @param frequency the target frequency of this Goertzel
 * @param Fs the sampling rate of the values accepted by update()
 */
class SimpleGoertzel(frequency: Double, Fs: Double, N: Int)
{
    private val k       = (0.5 + frequency / Fs * N).toInt
    private val w       = 2.0 * math.Pi / N * k
    private val wr      = math.cos(w)
    private val wr2     = 2.0 * wr
    private val wi      = math.sin(w)
    private val damping = 0.997

    private var prev    = 0.0
    private var prev2   = 0.0

    def X = Complex(wr * prev - prev2,  wi * prev)

    //faster for power spectrum
    def mag = prev * prev + prev2 * prev2
    
    //correct
    def mag2 = prev * prev + prev2 * prev2 - wr2 * prev * prev2

    /**
     * Real-valued Goertzel, with a complex as the result cpx.abs and cpx.arg have the power
     * and phase info     
     *               
     * @see https://en.wikipedia.org/wiki/Goertzel_algorithm
     */         
    def update(point: Double) : Unit =
        {
        val s = point + (prev * wr2 - prev2)
        prev2 = prev - point
        prev  = s
        }

}

class ComplexGoertzel(frequency: Double, Fs: Double, N: Int)
{
    private val k       = (0.5 + frequency / Fs * N).toInt
    private val w       = 2.0 * math.Pi / N * k
    private val wr      = math.cos(w)
    private val wr2     = 2.0 * wr
    private val damping = 0.97

    private var pr1   = 0.0
    private var pr2   = 0.0
    private var pi1   = 0.0
    private var pi2   = 0.0

    def mag = pr1*pr1 + pi1*pi1
    
    def update(point: Complex) : Unit =
        {
        val r = point.r + (pr1 * wr2 - pr2)
        pr2 = pr1 - point.r
        pr1  = r * damping
        val i = point.r + (pi1 * wr2 - pi2)
        pi2 = pi1 - point.r
        pi1  = i * damping
        }

}



/**
 * A single bin of a Goertzel array
 * @param wr the real-valued angle of this bin
 * @param wi the imaginary-valued angle of this bin
 */
case class GoertzelBin(wr: Double, wi: Double)
{
    var prev  = 0.0
    var prev2 = 0.0
    
    private val wr2     = wr * 2.0
    private val damping = 0.997 //just like an IIR filter
    
    def X = Complex(wr * prev - prev2, wi * prev)
 
    //faster for power spectrum
    def mag = prev * prev + prev2 * prev2
    
    //correct
    def mag2 = prev * prev + prev2 * prev2 - wr2 * prev * prev2
 
    def update(point: Double) =
        {
        val s = point + damping * (prev * wr2 - prev2)
        prev2 = prev
        prev  = s
        }
}


/**
 * The Goertzel transform.  Obviously this is simply a container
 * for the Goertzel bins, which do the actual processing.  This demonstrates
 * how wonderfully simple it is.
 *
 * @param bins the array of bins that this transform will contain
 */
class Goertzel(bins: Array[GoertzelBin]) extends ZTransform
{
    def X =
        bins.map(_.X)

    def powerSpectrum = 
        bins.map(_.mag)

    def update(point: Double)  =
        bins.foreach(_.update(point))
}


/**
 * Tools for creating Goertzel transforms.
 */
object Goertzel
{
    /**
     * Create a Goertzel transform with a desired number of bins between
     *     two frequencies
     * @param bins the number of desired bins
     * @param f1 the lower bound of the frequency band
     * @param f2 the upper bound of the frequency band
     * @param Fs the sample frequency of the incoming samples
     * @return a Goertzel transform that calculates the bins
     */
    def apply(bins: Int, f1: Double, f2: Double, Fs: Double) : Goertzel =
        {
        val binWidth = (f2 - f1) / bins
        var f = f1
        val xs = Array.fill(bins)
            {
            val omega = 2.0 * math.Pi * f / Fs
            val b = GoertzelBin(math.cos(omega), math.sin(omega))
            f += binWidth
            b
            }
        new Goertzel(xs)
        }
}






/**
 * Attempt to provide a fairly efficient Sliding DFT, where
 * the output bins are updated for each sample taken.  To save
 * processing, we only compute the number of bins that we need,
 * not necessarily the entire resolution.
 * 
 * Because of this specialization of the computation, this
 * transform algorithm can in a some cases be much more efficient
 * than a well-made FFT.
 * 
 * Consider a DFT with resolution N, meaning N virtual bins.
 * To perform this, when a sample x(i) is processed, each
 * bin is updated by adding x(i), subtracting x(i-N), and multiplying
 * by that bin's complex phase W(i). 
 * 
 * x(i-N) means the value N samples ago.  To remember what x(i-N) 
 * was, we keep a queue N samples long and remove the last for each 
 * one we add.
 * 
 * For example, say the sample rate is 40,000 samples per second.
 * SlidingRealDft(4000, 300) means that we want to break
 * up the sample rate into 4000 slices of 10Hz each, but we only
 * want to compute the first 300 of them, from 0 to 3000Hz. 
 * 
 * @param resolution the total number of virtual bins into which the
 * sample frequency is divided
 * 
 * @param size the actual number of desired bins, starting from 0Hz
 */
class SlidingComplexDft(resolution: Int, size: Int) extends ZTransform
{
    def this(resolution: Int) = this(resolution, resolution)
    
    private val pX = Array.fill(size)(Complex(0.0))
    
    private val omega = math.Pi * 2.0 / resolution.toDouble

    private val W = Array.tabulate(size)(i =>
       Complex(math.cos(omega * i), math.sin(omega * i)))
        
    private val queue =  Array.fill(resolution)(Complex(0.0))
    private var qptr = 0
    
    def X = pX.clone
    
    def powerSpectrum : Array[Double] =
        pX.map(_.mag)

    def update(point: Complex) : Unit =
        {
        val diff = point - queue(qptr)
        queue(qptr) = point
        qptr += 1
        if (qptr >= resolution)
            qptr = 0
        for (i <- 0 until size)
            pX(i) = (pX(i) + diff) * W(i)
        }
}



/**
 * Same as SlidingComplexDft, but works on real-valued samples.
 * 
 * @param resolution the total number of virtual bins into which the
 * sample frequency is divided
 * 
 * @param size the actual number of desired bins, starting from 0Hz
 */
class SlidingRealDft(resolution: Int, size: Int) extends ZTransform
{
    def this(resolution: Int) = this(resolution, resolution)
    
    private val pX = Array.fill(size)(Complex(0.0))
    
    private val omega = math.Pi * 2.0 / resolution.toDouble

    private val W = Array.tabulate(size)(i =>
       Complex(math.cos(omega * i), math.sin(omega * i)))
    
    private val queue = Array.fill(resolution)(0.0)
    private var qptr = 0
    
    def X = 
        pX.clone
        
    def powerSpectrum : Array[Double] =
        pX.map(_.mag)

    def update(point: Double) : Unit =
        {
        val diff = point - queue(qptr)
        queue(qptr) = point
        qptr += 1
        if (qptr >= resolution)
            qptr = 0
        for (i <- 0 until size)
            {
            val xk = pX(i)
            pX(i) = Complex(xk.r + diff, xk.i) * W(i)
            }
        }
}



/**
 * This version of the SlidingRealDft saves a little cpu by not working
 * explicitly with Complex numbers.  This may or may not be useful for your
 * purpose.  Try each and see how it works for you.
 * 
 * @param resolution the total number of virtual bins into which the
 * sample frequency is divided
 * 
 * @param size the actual number of desired bins, starting from 0Hz
 */
class SlidingRealDft2(resolution: Int, size: Int) extends ZTransform
{
    def this(resolution: Int) = this(resolution, resolution)
    
    private val Xr = Array.fill(size)(0.0)
    private val Xi = Array.fill(size)(0.0)
    
    private val omega = math.Pi * 2.0 / resolution.toDouble

    val Wr = Array.tabulate(size)(i => math.cos(omega * i))
    val Wi = Array.tabulate(size)(i => math.sin(omega * i))
    
    private val queue = Array.fill(resolution)(0.0)
    private var qptr = 0
    
    def X : Array[Complex] =
        {
        val xarr = Array.ofDim[Complex](size);
        for (j <- 0 until size)
            xarr(j) = Complex(Xr(j), Xi(j))
        xarr
        }

    def powerSpectrum : Array[Double] =
        Array.tabulate[Double](size)(j =>
            {
            val r = Xr(j)
            val i = Xi(j)
            r * r + i * i
            })

    def update(point: Double) : Unit =
        {
        val diff = point - queue(qptr)
        queue(qptr) = point
        qptr += 1
        if (qptr >= resolution)
            qptr = 0
        for (j <- 0 until size)
            {
            val r  = Xr(j) + diff
            val i  = Xi(j)
            val wr = Wr(j)
            val wi = Wi(j)
            Xr(j)  = r * wr - i * wi 
            Xi(j)  = i * wr + r * wi 
            }
        }
}


/**
 * Each Bin has its real and complex "twist" (exp(-j*Pi*n/N)) and its current value
 *
 * @param wr the real-valued (cosine) twist for this bin
 * @param wi the imaginary-valued (sine) twist for thsi bin
 */ 
case class SimpleDftBin(wr: Double, wi: Double)
{
    var xr = 0.0
    var xi = 0.0
    
    def update(diff: Double) =
        {
        val r = xr + diff
        val i = xi
        xr = r * wr - i * wi
        xi = i * wr + r * wi
        }
}

/**
 * This is an extremely simple version of the Sliding DFT that only calculates
 * the desired subset of bins of the set of all N bins of the transform.
 *
 * @param N the resolution of the DFT
 * @param bins an array of all of the desired bins.
 */
class SimpleDft(val N: Int, val bins: Array[SimpleDftBin]) extends ZTransform
{
    protected val queue = Array.fill(N)(0.0)
    protected var qptr = 0
    
    /**
     * Return the curent values of the bins
     */ 
    def X : Array[Complex] =
        bins.map(b=> Complex(b.xr, b.xi))

    /**
     * TODO: This seems terribly inefficient to calculate this for every call.
     *    Can't a running PS be maintained?
     */
    def powerSpectrum : Array[Double] =
        bins.map(b => b.xr * b.xr + b.xi * b.xi)

    /**
     * Add one sample to the DFT.  This will update all of the bins of interest
     */
    def update(point: Double) : Unit =
        {
        val diff    = point - queue(qptr)
        queue(qptr) = point
        qptr        = (qptr + 1) % N 
        for (b <- bins)
            b.update(diff)
        }
}

class SimpleDftWithDecimation(N: Int, bins: Array[SimpleDftBin], decimation: Int)
       extends SimpleDft(N, bins)
{
    var counter = 0
    
    /**
     * Add one sample to the DFT.  This will update all of the bins of interest
     */
    override def update(point: Double) : Unit =
        {
        counter += 1
        if (counter >= decimation)
            {
            val diff = point - queue(qptr)
            counter = 0
            for (b <- bins)
                b.update(diff)
            }
        queue(qptr) = point
        qptr = (qptr + 1) % N 
        }
}


class SimpleDftWithWindow(N: Int, bins: Array[SimpleDftBin], window: Window)
       extends SimpleDft(N, bins)
{
    var w = window(N)
    
    /**
     * Add one sample to the DFT.  This will update all of the bins of interest
     */
    override def update(point: Double) : Unit =
        {
        val diff = (point - queue(qptr)) * w(qptr)
        for (b <- bins)
            b.update(diff)
        queue(qptr) = point
        qptr = (qptr + 1) % N 
        }
}


object SimpleDft
{
    /**
     * Create a SimpleDFT that will process bins for a given resolution N and
     * sample frequency Fs.  Note that a limitation of the Sliding DFT is that
     * all of the calculated bins must be integer multiples of the bin width (Fs/N).
     * Thus the number of bins is not a parameter, but calculated.  The closest bins
     * to the bounding frequencies are chosen as endpoints.  Note that resolution N below is
     * calculated.This is the number of all of the virtual bins of the transform, even if
     * not all are processed. N bins of the bin width cover the sampling rate, Fs.
     *
     * @param nrBins this is the required of bins returned by this function
     * @param f1 the lower bounding frequency of the band of interest (in Hz)
     * @param f2 the high bounding frequency of the band of interest (in Hz)
     * @param Fs the sample frequency in Hz.
     * @param window the windowing curve for pre-treating the data 
     * @return a SimpleDFT that can calculate the Sliding DFT of the incoming samples.
     */
    def apply(nrBins: Int, f1: Double, f2: Double, Fs: Double,
         window: Window = Window.Rectangle) : SimpleDft =
        {
        val sampleRate = Fs
        //Get nearest integer multiples of the bounding frequencies.
        val binWidth = (f2 - f1) / nrBins
        val N        = (sampleRate / binWidth).toInt
        val bin1     = math.round(f1 / binWidth).toInt
        val bin2     = math.round(f2 / binWidth).toInt
        val bins     = bin2 - bin1
        val f0       = binWidth * bin1
        
        var f = f0
        val binList = Array.fill(bins)(
            {
            val omega  = f * 2.0 * math.Pi / sampleRate
            var wr     = math.cos(omega)
            val wi     = -math.sin(omega)
            val b      = SimpleDftBin(wr, wi)
            f         += binWidth
            b
            })
        if (window != Window.Rectangle)
            new SimpleDftWithWindow(N, binList, window)
        else
            new SimpleDft(N, binList)
        }
}

class SlidingDht(W: Array[Double])
{
    val size   = W.size
    val Xprev  = Array.fill(size)(0.0)
    val X      = Array.fill(size)(0.0)
    val buf    = Array.fill(size)(0.0)
    var bufptr = 0
    
    def powerSpectrum = X.map(v => math.abs(v))
    
    def update(v: Double) =
        {
        val diff = v - buf(bufptr)
        bufptr = (bufptr + 1) % size
        for (i <- 0 until size)
            {
            val tmp = X(i)
            X(i) += W(i) * diff - Xprev(i)
            Xprev(i) = tmp
            } 
        }
    

}


object SlidingDht
{
    def apply(nrBins: Int, f1: Double, f2: Double, Fs: Double) : SlidingDht =
        {
        val sampleRate = Fs
        //Get nearest integer multiples of the bounding frequencies.
        val binWidth = (f2 - f1) / nrBins
        val N        = (sampleRate / binWidth).toInt
        val bin1     = math.round(f1 / binWidth).toInt
        val bin2     = math.round(f2 / binWidth).toInt
        val bins     = bin2 - bin1
        val f0       = binWidth * bin1
        
        var f = f0
        val W = Array.fill(bins)(
            {
            val omega  = f * 2.0 * math.Pi / sampleRate
            val b      = math.cos(omega) + math.sin(omega)
            f         += binWidth
            b
            })
        new SlidingDht(W)
        }
}



import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}
import org.apache.commons.math3.complex.{Complex => JComplex}

/**
 * A traditional Fast Fourier Transform
 */
class Fft
{
    private val trans = new FastFourierTransformer(DftNormalization.STANDARD)

    def forward(points: Array[Double]) : Array[Complex] =
        {
        val t = trans.transform(points, TransformType.FORWARD)
        t.map(v=> Complex(v.getReal, v.getImaginary))
        }

    def inverse(points: Array[Double]) : Array[Complex] =
        {
        val t = trans.transform(points, TransformType.INVERSE)
        t.map(v=> Complex(v.getReal, v.getImaginary))
        }

    def forward(points: Array[Complex]) : Array[Complex] =
        {
        val jpoints = points.map(c=> new JComplex(c.r, c.i))
        val t = trans.transform(jpoints, TransformType.FORWARD)
        var res = t.map(c=> Complex(c.getReal, c.getImaginary))
        res
        }

    def inverse(points: Array[Complex]) : Array[Complex] =
        {
        val jpoints = points.map(c=> new JComplex(c.r, c.i))
        val t = trans.transform(jpoints, TransformType.INVERSE)
        var res = t.map(c=> Complex(c.getReal, c.getImaginary))
        res
        }

    def correlate(points: Array[Complex]) : Array[Complex] =
        {
        val jpoints = points.map(c=> new JComplex(c.r, c.i))
        val t = trans.transform(jpoints, TransformType.FORWARD)
        //multiply each cpx by its conjugate, making a real
        val t2 = t.map(c => c.multiply(c.conjugate))

        val t3 = trans.transform(t2, TransformType.INVERSE)
        val res = t3.map(c => Complex(c.getReal, c.getImaginary))
        res
        }
    
}

import edu.emory.mathcs.jtransforms.dht.DoubleDHT_1D
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D

class Fht(N: Int)
{
    private val trans = new DoubleDHT_1D(N)

    def forward(arr: Array[Double]) = trans.forward(arr)
    def inverse(arr: Array[Double]) = trans.inverse(arr, false)
}

class DFft(N: Int)
{
    private val trans = new DoubleFFT_1D(N)

    def forward(arr: Array[Double]) = trans.realForward(arr)
    def inverse(arr: Array[Double]) = trans.realInverse(arr, false)
    
    def powerSpectrum(in: Array[Double], bins: Int) : Array[Double] =
        {
        forward(in)
        var idx = 0
        val out = Array.fill(bins)
            {
            val r = in(idx)
            val i = in(idx+1)
            idx += 2
            r * r + i * i
            }
        out
        }
        
    def powerSpectrum(in: Array[Double]) : Array[Double] =
        {
        powerSpectrum(in, in.size / 2)
        }

    def intPowerSpectrum(in: Array[Double], bins: Int) : Array[Int] =
        {
        forward(in)
        var idx = 0
        val out = Array.fill(bins)
            {
            val r = (in(idx) * 10.0).toInt
            val i = (in(idx+1) * 10.0).toInt
            idx += 2
            r * r + i * i
            }
        out
        }
        
}







/**
 * The generic wavelet 
 * @param scales coefficients of the scales; scaling function
 */
class Wavelet(scales: Array[Double])
{
    /**
     * minimal wavelength of wavelet and scaling coefficients
     */
    val wavelength = scales.size
  
    /**
     * coefficients of the wavelet; wavelet function
     */
    val coeffs = 
        {
        var sign = -1.0
        scales.reverse.map(v =>
            {
            sign = -sign
            v * sign
            })
        }

  
    /**
     */
    def forward(arrTime : Array[Double]) : Array[Double] =
        {
        val len = arrTime.size
        val arrHilb = Array.ofDim[Double](len)
        var k = 0
        var h = len >> 1

        for (i <- 0 until h) 
            {

            for (j <- 0 until wavelength) 
                {
                k = ( i << 1 ) + j
                while ( k >= len )
                    k -= len

                arrHilb(i)     += arrTime(k) * scales(j) // low pass filter - energy (approximation)
                arrHilb(i + h) += arrTime(k) * coeffs(j) // high pass filter - details 
                } // wavelet

            } // h

        arrHilb
        } // forward

    /**
     */
    def reverse(arrHilb : Array[Double]) : Array[Double] =
        {
        val len = arrHilb.size
        var arrTime = Array.ofDim[Double](len)
    
        var k = 0
        var h = len >> 1
        for (i <- 0 until h)
            {
            for (j <- 0 until wavelength)
                {
    
                k = ( i << 1 ) + j
                while (k >= len)
                    k -= len
    
                // adding up details times energy (approximation)
                arrTime(k) += ( arrHilb(i) * scales(j) + arrHilb(i + h) * coeffs(j) )
    
                } // wavelet
    
            } //  h
    
        arrTime
        } // reverse




} // class





/**
 * A fast wavelet transform.  Different wavelets can be plugged in
 * to suit a given task.
 *
 * @see #Wavelet for some canned transforms
 */
class WaveletTransform(wavelet: Wavelet)
{

    /**
     * Performs the 1-D forward transform for arrays of dim N from time domain to
     * Hilbert domain for the given array using the Fast Wavelet Transform (FWT)
     * algorithm.
     *
     * @param arrTime samples from the time domain
     * @return the transform in the Hilbert domain
     */
    def forward(arrTime : Array[Double]) : Array[Double] =
        {
        var arrHilb = arrTime.clone
        var level = 0
        var h = arrTime.length;
        val minWaveLength = wavelet.wavelength
        if (h >= minWaveLength)
            {
            while ( h >= minWaveLength )
                {
                val iBuf = Array.ofDim[Double](h)
                Array.copy(arrHilb, 0, iBuf, 0, h)
                val oBuf = wavelet.forward(iBuf)
                Array.copy(oBuf, 0, arrHilb, 0, h)
                h = h >> 1
                level += 1
                }
            }
        arrHilb
        }

    /**
     * Performs the 1-D reverse transform for arrays of dim N from Hilbert domain
     * to time domain for the given array using the Fast Wavelet Transform (FWT)
     * algorithm and the selected wavelet.
     * 
     * @param arrHilb transformed from the Hilbert domain
     * @return the transform in the time domain
     */
    def reverse(arrHilb : Array[Double]) : Array[Double] =
        {
        var arrTime = arrHilb.clone
        var level = 0
        val minWaveLength = wavelet.wavelength
        var h = minWaveLength
        if (arrHilb.length >= minWaveLength) 
            {
            while (h <= arrTime.length && h >= minWaveLength)
                {
                val iBuf = Array.ofDim[Double](h)
                Array.copy(arrTime, 0, iBuf, 0, h)
                val oBuf = wavelet.reverse(iBuf)
                Array.copy(oBuf, 0, arrTime, 0, h)
                h = h << 1
                level += 1
                }
            }
        arrTime
        }

    /**
     * Performs the 1-D forward transform for arrays of dim N from time domain to
     * Hilbert domain for the given array using the Fast Wavelet Transform (FWT)
     * algorithm. The number of transformation levels applied is limited by
     * threshold.
     *
     * @param arrTime samples from the time domain
     * @param toLevel the lower limit of the transform
     * @return the transform in the Hilbert domain
     */
    def forward(arrTime : Array[Double], toLevel : Int ) =
        {
        var arrHilb = arrTime.clone
        var level = 0
        var h = arrTime.length
        var minWaveLength = wavelet.wavelength
        if (h >= minWaveLength)
            {

            while (h >= minWaveLength && level < toLevel)
                {
                val iBuf = Array.ofDim[Double](h)
                Array.copy(arrHilb, 0, iBuf, 0, h)
                val oBuf = wavelet.forward(iBuf)
                Array.copy(oBuf, 0, arrHilb, 0, h)
                h = h >> 1
                level += 1
                }

            }

        arrHilb
        }

    /**
     * Performs the 1-D reverse transform for arrays of dim N from Hilbert domain
     * to time domain for the given array using the Fast Wavelet Transform (FWT)
     * algorithm and the selected wavelet. The number of transformation levels
     * applied is limited by threshold.
     *
     * @param arrHilb transformed from the Hilbert domain
     * @param fromLevel the lower level of the transform
     * @return the transform in the time domain
     */
    def reverse(arrHilb : Array[Double], fromLevel : Int) : Array[Double] =
        {
        var arrTime = arrHilb.clone
        var level = 0
        val minWaveLength = wavelet.wavelength
    
        var h = ( arrHilb.length / (scala.math.pow( 2.0, fromLevel.toDouble - 1.0 ) ) ).toInt
    
        if (arrHilb.length >= minWaveLength)
            {

            while (h <= arrTime.length && h >= minWaveLength && level < fromLevel)
                {
                val iBuf = Array.ofDim[Double](h)
                Array.copy(arrTime, 0, iBuf, 0, h)
                val oBuf = wavelet.reverse(iBuf)
                Array.copy(oBuf, 0, arrTime, 0, h)
                h = h << 1
                level += 1
                }

            }

        arrTime
        }

} // WaveletTransform


/**
 * Some canned common wavelets
 */
object Wavelet
{
    private val norm = 1.0 / math.sqrt(2.0)
    private def nvals(vals : Double*) : Array[Double] =
        vals.map(_*norm).toArray
    
    /**
     * The most basic wavelet
     */
    lazy val haar = new Wavelet(nvals(
         1.0,
         1.0
         ))
    def haarTransform = new WaveletTransform(haar)
        
    /**
     * @see http://en.wikipedia.org/wiki/Daubechies_wavelet
     */
    lazy val daub4 = new Wavelet(nvals(
         0.6830127,
         1.1830127,
         0.3169873,
        -0.1830127
        ))
    def daub4Transform = new WaveletTransform(daub4)
        
    /**
     * @see http://en.wikipedia.org/wiki/Daubechies_wavelet
     */
    lazy val daub6 = new Wavelet(nvals(
         0.47046721,
         1.14111692,
         0.65036500,
        -0.19093442,
        -0.12083221,
         0.04981750
        ))
    def daub6Transform = new WaveletTransform(daub6)
        
    /**
     * @see http://en.wikipedia.org/wiki/Daubechies_wavelet
     */
    lazy val daub8 = new Wavelet(nvals(
         0.32580343,
         1.01094572,
         0.89220140,
        -0.03967503,
        -0.26450710,
         0.04361630,
         0.04650360,
        -0.01498699
        ))
    def daub8Transform = new WaveletTransform(daub8)
        
    /**
     * @see http://en.wikipedia.org/wiki/Daubechies_wavelet
     */
    lazy val daub10 = new Wavelet(nvals(
         0.22641898,
         0.85394354,
         1.02432694,
         0.19576696,
        -0.34265671,
        -0.04560113,
         0.10970265,
        -0.00882680,
        -0.01779187,
         4.71742793e-3
         ))
    def daub10Transform = new WaveletTransform(daub10)
        
}





