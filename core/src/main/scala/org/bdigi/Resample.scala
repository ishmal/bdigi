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

abstract class Resampler(f: (Double) => Unit)
{
    var d0 = 0.0
    var d1 = 0.0
    var d2 = 0.0
    var sum = 0.0

    var phase : Resampler.Phase = _

    def decimate(v: Double) =
    {
        d0 = d1 ; d1 = d2 ; d2 = v
        phase
    }
    
    def interpolate(v: Double) : Unit
}


/**
 * A highly experimental resampler hardcoded with a decimation of 6 and 3 coefficients
 * per phase.  Probably won't work!  :)
 */
object Resampler
{
    
    //#########################################################
    //###  DECIMATION : 2
    //#########################################################
    private val c0200 = -0.00000
    private val c0201 = 6.73393e-18
    private val c0202 = 0.287914
    private val c0203 = 0.452254
    private val c0204 = 0.109973
    private val c0205 = 0.00000

    //#########################################################
    //###  DECIMATION : 3
    //#########################################################
    private val c0300 = -0.00000
    private val c0301 = -0.00665934
    private val c0302 = 0.0318310
    private val c0303 = 0.181130
    private val c0304 = 0.318310
    private val c0305 = 0.271694
    private val c0306 = 0.106103
    private val c0307 = 0.00932308
    private val c0308 = -0.00000

    //#########################################################
    //###  DECIMATION : 4
    //#########################################################
    private val c0400 = -0.00000
    private val c0401 = -0.00357305
    private val c0402 = 2.84852e-18
    private val c0403 = 0.0428519
    private val c0404 = 0.131690
    private val c0405 = 0.220520
    private val c0406 = 0.244937
    private val c0407 = 0.186237
    private val c0408 = 0.0909025
    private val c0409 = 0.0219296
    private val c0410 = 7.73526e-19
    private val c0411 = -0.00000

    //#########################################################
    //###  DECIMATION : 5
    //#########################################################
    private val c0500 = -0.00000
    private val c0501 = -0.00196172
    private val c0502 = -0.00336679
    private val c0503 = 0.00849726
    private val c0504 = 0.0449745
    private val c0505 = 0.103355
    private val c0506 = 0.163178
    private val c0507 = 0.196726
    private val c0508 = 0.186985
    private val c0509 = 0.139359
    private val c0510 = 0.0778281
    private val c0511 = 0.0286021
    private val c0512 = 0.00411497
    private val c0513 = -0.000885547
    private val c0514 = -0.00000

    //#########################################################
    //###  DECIMATION : 6
    //#########################################################
    private val c0600 = -0.00000
    private val c0601 = -0.00116344
    private val c0602 = -0.00296700
    private val c0603 = 1.80051e-18
    private val c0604 = 0.0144470
    private val c0605 = 0.0438880
    private val c0606 = 0.0850224
    private val c0607 = 0.127510
    private val c0608 = 0.157800
    private val c0609 = 0.165248
    private val c0610 = 0.147236
    private val c0611 = 0.110447
    private val c0612 = 0.0675699
    private val c0613 = 0.0312787
    private val c0614 = 0.00882135
    private val c0615 = 8.47823e-19
    private val c0616 = -0.000767670
    private val c0617 = -0.00000

    //#########################################################
    //###  DECIMATION : 7
    //#########################################################
    private val c0700 = -0.00000
    private val c0701 = -0.000738756
    private val c0702 = -0.00222959
    private val c0703 = -0.00194649
    private val c0704 = 0.00376483
    private val c0705 = 0.0180421
    private val c0706 = 0.0417122
    private val c0707 = 0.0722011
    private val c0708 = 0.103761
    private val c0709 = 0.129071
    private val c0710 = 0.141661
    private val c0711 = 0.138195
    private val c0712 = 0.119674
    private val c0713 = 0.0910713
    private val c0714 = 0.0595247
    private val c0715 = 0.0318653
    private val c0716 = 0.0124668
    private val c0717 = 0.00224596
    private val c0718 = -0.000901830
    private val c0719 = -0.000571381
    private val c0720 = -0.00000


    type Phase = () => Unit 
    
    class Resampler2(f: (Double) => Unit) extends Resampler(f)
    {
        private val p00 : Phase = () => { sum   = d0 * c0200 + d1 * c0202 + d2 * c0204  ; phase = p01 }
        private val p01 : Phase = () => { f(sum + d0 * c0201 + d1 * c0203 + d2 * c0205) ; phase = p00 }

        phase = p00;

        def interpolate(v: Double) =
        {
            d0 = d1 ; d1 = d2 ; d2 = v
            f(d0 * c0200 + d1 * c0202 + d2 * c0204)
            f(d0 * c0201 + d1 * c0203 + d2 * c0205)
        }
    }//Resampler2
        
    class Resampler3(f: (Double) => Unit) extends Resampler(f)
    {
        private val p00 : Phase = () => { sum   = d0 * c0300 + d1 * c0303 + d2 * c0306  ; phase = p01 }
        private val p01 : Phase = () => { sum  += d0 * c0301 + d1 * c0304 + d2 * c0307  ; phase = p02 }
        private val p02 : Phase = () => { f(sum + d0 * c0302 + d1 * c0305 + d2 * c0308) ; phase = p00 }

        phase = p00;

        def interpolate(v: Double) =
        {
            d0 = d1 ; d1 = d2 ; d2 = v
            f(d0 * c0300 + d1 * c0303 + d2 * c0306)
            f(d0 * c0301 + d1 * c0304 + d2 * c0307)
            f(d0 * c0302 + d1 * c0305 + d2 * c0308)
        }
    }//Resampler3
        
    class Resampler4(f: (Double) => Unit) extends Resampler(f)
    {
        private val p00 : Phase = () => { sum   = d0 * c0400 + d1 * c0404 + d2 * c0408  ; phase = p01 }
        private val p01 : Phase = () => { sum  += d0 * c0401 + d1 * c0405 + d2 * c0409  ; phase = p02 }
        private val p02 : Phase = () => { sum  += d0 * c0402 + d1 * c0406 + d2 * c0410  ; phase = p03 }
        private val p03 : Phase = () => { f(sum + d0 * c0403 + d1 * c0407 + d2 * c0411) ; phase = p00 }

        phase = p00;

        def interpolate(v: Double) =
        {
            d0 = d1 ; d1 = d2 ; d2 = v
            f(d0 * c0400 + d1 * c0404 + d2 * c0408)
            f(d0 * c0401 + d1 * c0405 + d2 * c0409)
            f(d0 * c0402 + d1 * c0406 + d2 * c0410)
            f(d0 * c0403 + d1 * c0407 + d2 * c0411)
        }
    }//Resampler4
        
    class Resampler5(f: (Double) => Unit) extends Resampler(f)
    {
        private val p00 : Phase = () => { sum   = d0 * c0500 + d1 * c0505 + d2 * c0510  ; phase = p01 }
        private val p01 : Phase = () => { sum  += d0 * c0501 + d1 * c0506 + d2 * c0511  ; phase = p02 }
        private val p02 : Phase = () => { sum  += d0 * c0502 + d1 * c0507 + d2 * c0512  ; phase = p03 }
        private val p03 : Phase = () => { sum  += d0 * c0503 + d1 * c0508 + d2 * c0513  ; phase = p04 }
        private val p04 : Phase = () => { f(sum + d0 * c0504 + d1 * c0509 + d2 * c0514) ; phase = p00 }

        phase = p00;

        def interpolate(v: Double) =
        {
            d0 = d1 ; d1 = d2 ; d2 = v
            f(d0 * c0500 + d1 * c0505 + d2 * c0510)
            f(d0 * c0501 + d1 * c0506 + d2 * c0511)
            f(d0 * c0502 + d1 * c0507 + d2 * c0512)
            f(d0 * c0503 + d1 * c0508 + d2 * c0513)
            f(d0 * c0504 + d1 * c0509 + d2 * c0514)
        }
    }//Resampler5
        
    class Resampler6(f: (Double) => Unit) extends Resampler(f)
    {
        private val p00 : Phase = () => { sum   = d0 * c0600 + d1 * c0606 + d2 * c0612  ; phase = p01 }
        private val p01 : Phase = () => { sum  += d0 * c0601 + d1 * c0607 + d2 * c0613  ; phase = p02 }
        private val p02 : Phase = () => { sum  += d0 * c0602 + d1 * c0608 + d2 * c0614  ; phase = p03 }
        private val p03 : Phase = () => { sum  += d0 * c0603 + d1 * c0609 + d2 * c0615  ; phase = p04 }
        private val p04 : Phase = () => { sum  += d0 * c0604 + d1 * c0610 + d2 * c0616  ; phase = p05 }
        private val p05 : Phase = () => { f(sum + d0 * c0605 + d1 * c0611 + d2 * c0617) ; phase = p00 }

        phase = p00;

        def interpolate(v: Double) =
        {
            d0 = d1 ; d1 = d2 ; d2 = v
            f(d0 * c0600 + d1 * c0606 + d2 * c0612)
            f(d0 * c0601 + d1 * c0607 + d2 * c0613)
            f(d0 * c0602 + d1 * c0608 + d2 * c0614)
            f(d0 * c0603 + d1 * c0609 + d2 * c0615)
            f(d0 * c0604 + d1 * c0610 + d2 * c0616)
            f(d0 * c0605 + d1 * c0611 + d2 * c0617)
        }
    }//Resampler6
        
    class Resampler7(f: (Double) => Unit) extends Resampler(f)
    {
        private val p00 : Phase = () => { sum   = d0 * c0700 + d1 * c0707 + d2 * c0714  ; phase = p01 }
        private val p01 : Phase = () => { sum  += d0 * c0701 + d1 * c0708 + d2 * c0715  ; phase = p02 }
        private val p02 : Phase = () => { sum  += d0 * c0702 + d1 * c0709 + d2 * c0716  ; phase = p03 }
        private val p03 : Phase = () => { sum  += d0 * c0703 + d1 * c0710 + d2 * c0717  ; phase = p04 }
        private val p04 : Phase = () => { sum  += d0 * c0704 + d1 * c0711 + d2 * c0718  ; phase = p05 }
        private val p05 : Phase = () => { sum  += d0 * c0705 + d1 * c0712 + d2 * c0719  ; phase = p06 }
        private val p06 : Phase = () => { f(sum + d0 * c0706 + d1 * c0713 + d2 * c0720) ; phase = p00 }

        phase = p00;

        def interpolate(v: Double) =
        {
            d0 = d1 ; d1 = d2 ; d2 = v
            f(d0 * c0700 + d1 * c0707 + d2 * c0714)
            f(d0 * c0701 + d1 * c0708 + d2 * c0715)
            f(d0 * c0702 + d1 * c0709 + d2 * c0716)
            f(d0 * c0703 + d1 * c0710 + d2 * c0717)
            f(d0 * c0704 + d1 * c0711 + d2 * c0718)
            f(d0 * c0705 + d1 * c0712 + d2 * c0719)
            f(d0 * c0706 + d1 * c0713 + d2 * c0720)
        }
    }//Resampler7
    
    
    def apply(decimation: Int)(f: (Double)=> Unit) : Resampler =
        {
        decimation match
            {
            case 2 => new Resampler2(f)
            case 3 => new Resampler3(f)
            case 4 => new Resampler4(f)
            case 5 => new Resampler5(f)
            case 6 => new Resampler6(f)
            case 7 => new Resampler7(f)
            case _ => throw new IllegalArgumentException("Decimation " + decimation + " not supported")
            }
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

