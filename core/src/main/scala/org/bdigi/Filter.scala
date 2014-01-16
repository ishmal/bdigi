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


/**
 * This file contains various filters for various purposes. New ones will
 * be added here as they are coded.
 */  


//########################################################################
//#  F I R
//########################################################################


class Fir(val coeffs: Array[Double])
{
    private val size       = coeffs.size
    private val minus1     = size - 1
    private val delayLine  = Array.fill(size)(0.0)
    private val delayLineC = Array.fill(size)(Complex(0.0))
    private var delayIndex = 0
    
    
    def update(sample: Double) : Double =
        {
        delayIndex = (delayIndex + minus1) % size  //walk backward
        delayLine(delayIndex) = sample
        var sum = 0.0
        var idx = delayIndex
        for (coeff <- coeffs)
            {
            val v = delayLine(idx)
            idx = (idx + 1) % size
            sum += v * coeff
            }
        sum
        }
        
    def update(sample: Complex) : Complex =
        {
        delayIndex = (delayIndex + minus1) % size
        delayLineC(delayIndex) = sample
        var realsum = 0.0
        var imagsum = 0.0
        var idx = delayIndex
        for (coeff <- coeffs)
            {
            val v = delayLineC(idx)
            idx = (idx + 1) % size
            realsum += v.r * coeff
            imagsum += v.i * coeff
            }
        Complex(realsum, imagsum)
        }
}



/**
 * Simply generates the coefficients for a given type and size
 */ 
trait Window
{
    def apply(indx: Int) : Array[Double]
}

object Window
{
    private val twopi = 2.0 * math.Pi

    // some filter windows
    object Rectangle extends Window
        {
        def apply(size: Int) = Array.fill(size)(1.0)
        }

    object Hamming extends Window
        {
        def apply(size: Int) = Array.tabulate(size)(i=> 0.54 - 0.46 * math.cos(twopi * i / (size-1)))
        }

    object Hann extends Window
        {
        def apply(size: Int) = Array.tabulate(size)(i=> 0.5 - 0.5 * math.cos(twopi * i / (size-1)))
        }

    object Blackman extends Window
        {
        def apply(size: Int) = Array.tabulate(size)(i=> 
                 0.42 -
                 0.5  * math.cos(twopi         * i / (size-1)) +
                 0.08 * math.cos(4.0 * math.Pi * i / (size-1))
                 )
        }
}






/**
 * Factories for various types of FIR filters
 */
object Fir
{

    val twopi = 2.0 * math.Pi
    
    def genCoeffs(size: Int, window: Window)(f: (Double)=> Double) : Array[Double] =
        {
        val center = size.toDouble * 0.5
        val W = window(size)
        var sum = 0.0
        val arr = Array.tabulate(size) ( i =>
            {
            val v = f(i.toDouble-center) * W(i)
            sum += v
            v
            })
        arr.map( _ / sum )
        }



    //#############################################
    //# C O M M O N    F I L T E R S
    //#############################################

    def average(size: Int) =
        new Fir(Array.fill(size)(1.0/size))

    def boxcar(size: Int) =
        new Fir(Array.fill(size)(1.0))


    def lowPassCoeffs(size: Int, cutoffFreq: Double, sampleRate: Double, window: Window) =
        {
        val omega = 2.0 * math.Pi * cutoffFreq / sampleRate

        genCoeffs(size, window)(i =>
            {
            if (i == 0)
                omega / math.Pi 
            else 
                math.sin(omega * i) / (math.Pi * i)
            })
        }
        
    def lowPass(size: Int, cutoffFreq: Double, sampleRate: Double, window: Window = Window.Hamming) =
        new Fir(lowPassCoeffs(size, cutoffFreq, sampleRate, window))

    def highPassCoeffs(size: Int, cutoffFreq: Double, sampleRate: Double, window: Window) =
        {
        val omega  = 2.0 * math.Pi * cutoffFreq / sampleRate
        
        genCoeffs(size, window)(i =>
            {
            if (i == 0)
                1.0 - omega / math.Pi 
            else 
                -math.sin(omega * i) / (math.Pi * i)
            })
        }


    def highPass(size: Int, cutoffFreq: Double, sampleRate: Double, window: Window = Window.Hamming) =
        new Fir(highPassCoeffs(size, cutoffFreq, sampleRate, window))

    def bandPassCoeffs(size: Int, loCutoffFreq: Double, hiCutoffFreq: Double, sampleRate: Double,
         window: Window = Window.Hamming) =
        {
        val omega1 = 2.0 * math.Pi * loCutoffFreq / sampleRate
        val omega2 = 2.0 * math.Pi * hiCutoffFreq / sampleRate
        
        genCoeffs(size, window)(i =>
            {
            if (i == 0)
                (omega2 - omega1) / math.Pi
            else 
                (math.sin(omega2 * i) - math.sin(omega1 * i)) / (math.Pi * i)
            })
        }

    def bandPass(size: Int, loCutoffFreq: Double, hiCutoffFreq: Double, sampleRate: Double, window: Window = Window.Hamming) =
        new Fir(bandPassCoeffs(size, loCutoffFreq, hiCutoffFreq, sampleRate, window))

    def bandStopCoeffs(size: Int, loCutoffFreq: Double, hiCutoffFreq: Double,
         sampleRate: Double, window: Window = Window.Hamming) =
        {
        val omega1 = 2.0 * math.Pi * loCutoffFreq / sampleRate
        val omega2 = 2.0 * math.Pi * hiCutoffFreq / sampleRate
        
        genCoeffs(size, window)(i =>
            {
            if (i == 0)
                1.0 - (omega2 - omega1) / math.Pi
            else 
                (math.sin(omega1 * i) - math.sin(omega2 * i)) / (math.Pi * i)
            })
        }



    def bandStop(size: Int, loCutoffFreq: Double, hiCutoffFreq: Double, sampleRate: Double, window: Window = Window.Hamming) =
        new Fir(bandStopCoeffs(size, loCutoffFreq, hiCutoffFreq, sampleRate, window))

    //#############################################
    //# L E S S    C O M M O N    F I L T E R S
    //#############################################


    def gaussianCoeffs(size: Int, rolloff: Double, sampleRate: Double, window: Window = Window.Hamming) =
        {
        val a  = rolloff
    
        genCoeffs(size, window)(i =>
            {
            math.sqrt(a / math.Pi) * math.exp(-a * i * i)
            })
        }

    def gaussian(size: Int, rolloff: Double, sampleRate: Double, window: Window = Window.Hamming) =
        new Fir(gaussianCoeffs(size, rolloff, sampleRate, window))

    def hilbertCoeffs(size: Int, window: Window) =
        {
        genCoeffs(size, window)(i =>
            {
            if (i % 2 == 0)
                0.0
            else
                1.0 / (math.Pi * i) //everybody has a different one of these
            })
        }

    def hilbert(size: Int, window: Window = Window.Hamming) =
        new Fir(hilbertCoeffs(size, window))

    /**
     * It is suggested that size be >= 4 * (sampleRate/symbolFreq) + 1
     * @param roloff is the "tightness" of the curve.  [0..1] ..  0.35 is common 
     */ 
    def raisedCosineCoeffs(size: Int, rolloff: Double, symbolFreq: Double, 
                   sampleRate: Double, window: Window) =
        {
        val T  = sampleRate / symbolFreq
        val a  = rolloff
    
        genCoeffs(size, window)(i =>
            {
            val nT = i.toDouble / T
            val anT = a * nT
            if (i == 0)
                1.0
            else if (anT == 0.5 || anT == -0.5)//look at denominator below
                math.sin(math.Pi*nT)/(math.Pi*nT) * math.Pi / 4.0 
            else
                math.sin(math.Pi*nT)/(math.Pi*nT) * math.cos(math.Pi * anT) /
                        (1.0 - 4.0 * anT * anT)
            })
        }


    def raisedCosine(size: Int, rolloff: Double, symbolFreq: Double,
        sampleRate: Double, window: Window = Window.Hamming) =
        new Fir(raisedCosineCoeffs(size, rolloff, symbolFreq, sampleRate, window))

    def rootRaisedCosineCoeffs(size: Int, rolloff: Double, symbolFreq: Double, 
                   sampleRate: Double, window: Window) =
        {
        val T = sampleRate / symbolFreq
        val a = rolloff
    
        genCoeffs(size, window)(i =>
            {
            val nT = i.toDouble / T
            val anT = a * nT
            val pnT = math.Pi * nT
            if (i == 0)
                1.0 - a + 4.0 * a / math.Pi
            else if (i == T/(4.0*a) || i == -T/4.0*a)
                a / math.sqrt(2.0) * ((1.0 + 2.0 / math.Pi) * math.sin(math.Pi / (4.0 * a)) +
                    (1.0 - 2.0 / math.Pi) * math.cos(math.Pi / (4.0 * a)) )
            else
                (math.sin(pnT * (1.0 - a)) + 4.0 * anT * math.cos(pnT * (1.0 + a))) /
                        (pnT * (1.0 - 4.0 * anT * 4.0 * anT) )
            })
        }

    def rootRaisedCosine(size: Int, rolloff: Double, symbolFreq: Double,
        sampleRate: Double, window: Window = Window.Hamming) =
        new Fir(rootRaisedCosineCoeffs(size, rolloff, symbolFreq, sampleRate, window))


}



 
//########################################################################
//#  I I R
//########################################################################
/**
 * I just have extra-simple IIRs for now.  Need to expand upon these
 * later
 */  

class IirLp(frequency: Double, sampleRate: Double)
{
    val attenuation = sampleRate / (2.0 * math.Pi * frequency)
    val decay = 0.999
    val invAttenuation = 1.0 / attenuation * decay
    var vlp  = 0.0
    var vlpc = Complex(0.0)
    
    def update(v: Double) : Double =
        {
        vlp += ((v - vlp ) * invAttenuation)
        vlp
        }

    def update(v: Complex) : Complex =
        {
        vlpc += ((v - vlpc) * invAttenuation)
        vlpc
        }
}




class Iir2(a0: Double, a1: Double, a2: Double,
          b0: Double, b1: Double, b2: Double)
{
    var in1  = 0.0
    var in2  = 0.0
    var out1 = 0.0
    var out2 = 0.0
    
    def clear = 
        {
        in1  = 0.0
        in2  = 0.0
        out1 = 0.0
        out2 = 0.0
        }

   def update(v: Double) : Double =
        {
        val y = a0 * (b0 * v + b1 * in1 + b2 * in2 - a1 * out1 - a2 * out2)
        in2  = in1
        in1  = v
        out2 = out1
        out1 = y
        y
        }

}


object Iir2
{
    val twopi = math.Pi * 2.0

    def lowPass(frequency: Double, sampleRate: Double, q: Double = 0.707) : Iir2 =
        {
        val freq  = frequency * twopi / sampleRate
        val cs    = MathUtil.cossin(freq)
        val alpha = cs._2 / q
        val a0    = 1.0 / (1.0 + alpha)
        val a1    = -2.0 * cs._1
        val a2    = 1.0 - alpha
        val b0    = (1.0 - cs._1) * 0.5
        val b1    = 1.0 - cs._1
        val b2    = b0
        new Iir2(a0, a1, a2, b0, b1, b2)
        }

    def highPass(frequency: Double, sampleRate: Double, q: Double = 0.707) : Iir2 =
        {
        val freq  = frequency * twopi / sampleRate
        val cs    = MathUtil.cossin(freq)
        val alpha = cs._2 / q
        val a0    = 1.0 / (1.0 + alpha)
        val a1    = -2.0 * cs._1
        val a2    = 1.0 - alpha
        val b0    = (1.0 + cs._1) * 0.5
        val b1    = -(1.0 + cs._1)
        val b2    = b0
        new Iir2(a0, a1, a2, b0, b1, b2)
        }

    def bandPass(frequency: Double, sampleRate: Double, q: Double = 0.5) : Iir2 =
        {
        val freq  = frequency * twopi / sampleRate
        val cs    = MathUtil.cossin(freq)
        val alpha = cs._2 / q
        val a0    = 1.0 / (1.0 + alpha)
        val a1    = -2.0 * cs._1
        val a2    = 1.0 - alpha
        val b0    = cs._2 * 0.5
        val b1    = 0.0
        val b2    = -cs._2 * 0.5
        new Iir2(a0, a1, a2, b0, b1, b2)
        }
}





//########################################################################
//#  B I Q U A D
//########################################################################

/**
 * A biquad filter
 * @see http://en.wikipedia.org/wiki/Digital_biquad_filter
 */


class Biquad(b0: Double, b1: Double, b2: Double, a0: Double, a1: Double, a2: Double)
{
    var x1 = 0.0
    var x2 = 0.0 
    var y1 = 0.0
    var y2 = 0.0

    var x1c = Complex(0.0)
    var x2c = Complex(0.0) 
    var y1c = Complex(0.0)
    var y2c = Complex(0.0)

    def update(x: Double) : Double =
        {
        val y = x * a0 + x1 * a1 + x2 * a2 - y1 * b1 - y2 * b2
        x2 = x1 ; x1 = x ; y2 = y1 ; y1 = y
        y
        } 

    def update(x: Complex) : Complex =
        {
        val y = x * a0 + x1c * a1 + x2c * a2 - y1c * b1 - y2c * b2
        x2c = x1c ; x1c = x ; y2c = y1c ; y1c = y
        y
        } 

}



object Biquad
{
    def lowPass(frequency: Double, sampleRate: Double, q: Double = 0.707) =
        {
        val freq = 2.0 * math.Pi * frequency / sampleRate
        val alpha = math.sin(freq) / (2.0 * q)
        val b0 = (1.0 - math.cos(freq)) / 2.0
        val b1 =  1.0 - math.cos(freq)
        val b2 = (1.0 - math.cos(freq)) / 2.0
        val a0 = 1.0 + alpha
        val a1 = -2.0 * math.cos(freq)
        val a2 = 1.0 - alpha    
        new Biquad(b0, b1, b2, a0, a1, a2)
        }
        

    def highPass(frequency: Double, sampleRate: Double, q: Double = 0.707) =
        {
        val freq = 2.0 * math.Pi * frequency / sampleRate
        val alpha = math.sin(freq) / (2.0 * q)
        val b0 =  (1.0 + math.cos(freq)) / 2.0
        val b1 = -(1.0 + math.cos(freq))
        val b2 =  (1.0 + math.cos(freq)) / 2.0
        val a0 = 1.0 + alpha
        val a1 = -2.0 * math.cos(freq)
        val a2 = 1.0 - alpha    
        new Biquad(b0, b1, b2, a0, a1, a2)
        }

    def bandPass(frequency: Double, sampleRate: Double, q: Double = 0.5) =
        {
        val freq = 2.0 * math.Pi * frequency / sampleRate
        val alpha = math.sin(freq) / (2.0 * q)
        val b0 = math.sin(freq) / 2.0   // = q*alpha
        val b1 = 0.0
        val b2 = -math.sin(freq) / 2.0  // = -q*alpha
        val a0 = 1.0 + alpha
        val a1 = -2.0 * math.cos(freq)
        val a2 = 1.0 - alpha    
        new Biquad(b0, b1, b2, a0, a1, a2)
        }

    def bandReject(frequency: Double, sampleRate: Double, q: Double = 0.5) =
        {
        val freq = 2.0 * math.Pi * frequency / sampleRate
        val alpha = math.sin(freq) / (2.0 * q)
        val b0 = 1.0
        val b1 = -2.0 * math.cos(freq)
        val b2 = 1.0
        val a0 = 1.0 + alpha
        val a1 = -2.0 * math.cos(freq)
        val a2 = 1.0 - alpha    
        new Biquad(b0, b1, b2, a0, a1, a2)
        }

}

















