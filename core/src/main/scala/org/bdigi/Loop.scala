/**
 * Scala SDR tool
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (c) 2013 Bob Jamison
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
 * A costas loop with a given center freq and max drift. 
 */ 
class CostasLoop(frequency: Double, bandwidth: Double, sampleRate: Double)
{
    /**
     * frequency is in Hertz
     * freq is in radians per sample
     */
    var counter = 0
    
    val twopi = 2.0 * math.Pi
    val radiansPerSample = twopi / sampleRate
    val samplesPerRadian = 1.0 / radiansPerSample

    def toRadians(v: Double) : Double =
        v * radiansPerSample

    def toHertz(v: Double) : Double =
        v * samplesPerRadian
    
    // control loop settings
    val bw        = twopi / 200.0  //basically: 200 samples
    val damp      = 0.707
    val alpha     = (4.0 * damp * bw) / (1.0 + 2.0 * damp * bw + bw * bw)
    val beta      = (4.0 * bw   * bw) / (1.0 + 2.0 * damp * bw + bw * bw)

    // initialize
    var phase   = 0.0    
    var freq    = toRadians(frequency)
    val minFreq = toRadians(frequency - bandwidth * 0.5) //make it easy to hit
    val maxFreq = toRadians(frequency + bandwidth * 0.5)
    //println("freq:" + freq + " hertz: " + toHertz(freq))

    def po(msg: String) =
        {
        if (counter % 500 == 0) println(msg)
        }
    
    def update(v: Double) : Complex =
        {
        val cs    = MathUtil.cossin(phase)
        var iq    = Complex(v * cs._1, -v * cs._2)
        //var err   = -iq.i * math.signum(iq.r)
        var err   = iq.r * math.signum(iq.i) + iq.i * math.signum(iq.r)
        //var err   = -iq.i * iq.r
        
        val limit = 1.0
        if (err > limit)
            err = limit
        else if (err < -limit)
            err = -limit
            
        freq  = freq  + beta * err
        phase = phase + freq + alpha * err
        if (freq > maxFreq)
            freq = maxFreq
        else if (freq < minFreq)
            freq = minFreq         
        while (phase > twopi)
            phase -= twopi
        while (phase < -twopi)
            phase += twopi           
             
        //po("freq: " + toHertz(f1) + " corr:" + (toHertz(freq) - frequency))
        counter += 1
        iq
        }

    def update(data: Array[Double]) : Array[Complex] =
        data.map(update)

}



/**
 * A costas loop with a given center fres and max drift. 
 */ 
class CostasLoop2(frequency: Double, bandwidth: Double, sampleRate: Double)
{
    val ctrlLoop = new ControlLoop(frequency, bandwidth, sampleRate)
    
    def update(v: Double) : Complex =
        {
        var iq = ctrlLoop.update(v)(iq =>
            {
            val i = iq.r * math.signum(iq.i) 
            val q = iq.i * math.signum(iq.r) 
            var err = i - q

            val limit = 0.5
            if (err > limit)
                err = limit
            else if (err < -limit)
                err = -limit
            err
            })
        
        iq
        }

    def update(data: Array[Double]) : Array[Complex] =
        data.map(update)

}




/**
 * Use this class as a frequency difference detector
 */ 
class Afc(frequency: Double, bandwidth: Double, sampleRate: Double)
{
    val twopi = 2.0 * math.Pi
    val radiansPerSample = twopi / sampleRate
    val samplesPerRadian = 1.0 / radiansPerSample
    val freq = frequency * radiansPerSample
    var phase = 0.0
    
    val lpfI = Fir.lowPass(21, frequency, sampleRate)
    val lpfQ = Fir.lowPass(21, frequency, sampleRate)
    val outf = Fir.average(31)
    
    var counter = 0
    
    var lastI = 0.0
    
    var out = 0.0

    def update(v: Double) : Double =
        {
        val cs = MathUtil.cossin(phase)
        var i =  v * cs._1
        var q = -v * cs._2
        i = lpfI.update(i)
        val dt = i - lastI
        lastI = i
        i = math.signum(dt)
        q = lpfQ.update(q)
        q = math.signum(q)
        out = i * q
        out = outf.update(out)

        phase += freq
        if (phase >= twopi)
            phase -= twopi
        
        counter += 1
        if (counter % 10000 == 0)
            println("diff: " + out)
            
        out
        }
    
    def update(arr: Seq[Double]) : Double =
        {
        arr.foreach(update)
        out
        }
}


/**
 * Code this like an IIR low pass filter.  But instead
 * of returning the filtered version of the signal,  use it
 * as a steady-state measure of the average amplitude.  Use this
 * to scale the signal appropriately
 */    
class Agc(frequency: Double, sampleRate: Double)
{
    val attenuation = sampleRate / (2.0 * math.Pi * frequency)
    val decay = 0.999
    val invAttenuation = 1.0 / attenuation * decay
    var vlp  = 0.0
    
    val desired = 1.0
    var gain = 1.0
    
    val min = 0.01
    val max = 100.0
    
    def feedback(v: Double) =
        {
        vlp += (v - vlp) * invAttenuation
        gain = desired / math.abs(vlp)
        if (gain < min) 
            gain = min
        else if (gain > max)
            gain = max
        //println("gain: " + gain)
        }

    def update(v: Complex) : Complex =
        v * gain
}

/**
 * Code this like an IIR low pass filter.  But instead
 * of returning the filtered version of the signal,  use it
 * as a steady-state measure of the average amplitude.  Use this
 * to scale the signal appropriately
 */    
class SlowAgc(milliseconds: Double, sampleRate: Double)
{
    val howManySamples = sampleRate * milliseconds * 0.001
    val attenuation = 1.0 / howManySamples
    
    var vlp  = 0.0
    
    val desired = 1.0

    var gain    = 1.0    
    val mingain = 0.01
    val maxgain = 100.0
    
    def feedback(v: Double) =
        {
        vlp += (v - vlp) * attenuation
        gain = desired / math.abs(vlp)
        if (gain < mingain) 
            gain = mingain
        else if (gain > maxgain)
            gain = maxgain
        //println("gain: " + gain)
        }

    def update(v: Complex) : Complex =
        v * gain
}



/**
 * A control loop used for various purposes
 */ 
class ControlLoop(frequency: Double, bandwidth: Double, sampleRate: Double)
{
    /**
     * frequency is in Hertz
     * freq is in radians per sample
     */
    var counter = 0
    
    val twopi = 2.0 * math.Pi
    val radiansPerSample = twopi / sampleRate
    val samplesPerRadian = 1.0 / radiansPerSample

    def toRadians(v: Double) : Double =
        v * radiansPerSample

    def toHertz(v: Double) : Double =
        v * samplesPerRadian
    
    // control loop settings
    val bw        = twopi / 200.0  //basically: N samples
    val damp      = 0.707
    val alpha     = (4.0 * damp * bw) / (1.0 + 2.0 * damp * bw + bw * bw)
    val beta      = (4.0 * bw   * bw) / (1.0 + 2.0 * damp * bw + bw * bw)

    // initialize
    var phase   = 0.0    
    var freq    = toRadians(frequency)
    val minFreq = toRadians(frequency - bandwidth * 0.5)
    val maxFreq = toRadians(frequency + bandwidth * 0.5)

    def po(msg: String) =
        {
        if (counter % 500 == 0) println(msg)
        }
    

    def update(v: Double)(errf: Complex=>Double = {iq=>0.0}): Complex =
        {
        val cs    = MathUtil.cossin(phase)
        var iq    = Complex(v * cs._1, v * cs._2)
                     
        val err = errf(iq)
        
        freq  = freq  + beta * err
        phase = phase + freq + alpha * err
        if (freq > maxFreq)
            freq = maxFreq
        else if (freq < minFreq)
            freq = minFreq         
        while (phase > twopi)
            phase -= twopi
        while (phase < -twopi)
            phase += twopi                       

        //po("freq: " + toHertz(f1) + " corr:" + (toHertz(freq) - frequency))
        counter += 1
        iq
        }

}



/**
 * A costas loop with a given center fres and max drift. 
 */ 
class GardnerLoop2(frequency: Double, sampleRate: Double)
{
    /**
     * frequency is in Hertz
     * freq is in radians per sample
     */
    var counter = 0
    
    val twopi = 2.0 * math.Pi
    val radiansPerSample = twopi / sampleRate
    val samplesPerRadian = 1.0 / radiansPerSample

    def toRadians(v: Double) : Double =
        v * radiansPerSample

    def toHertz(v: Double) : Double =
        v * samplesPerRadian
    
    // control loop settings
    val bw        = twopi / 100.0  //basically: 200 samples
    val damp      = 0.707
    val alpha     = (4.0 * damp * bw) / (1.0 + 2.0 * damp * bw + bw * bw)
    val beta      = (4.0 * bw   * bw) / (1.0 + 2.0 * damp * bw + bw * bw)

    // initialize
    var phase     = 0.0    
    var freq      = toRadians(frequency)
    val bandwidth = frequency * 0.1
    val minFreq   = toRadians(frequency - bandwidth) //make it easy to hit
    val maxFreq   = toRadians(frequency + bandwidth)
    println("freq:" + freq + " hertz: " + toHertz(freq))

    val samplesPerSymbol = sampleRate / frequency
    
    val buf = Array.fill(samplesPerSymbol.toInt)(Complex(0.0))
    var first = 0
    var last  = 1
    var half  = buf.size / 2
    
    def append(v: Complex) 
        {
        first = last
        last += 1
        if (last >= buf.size)
            last = 0
        half += 1
        if (half >= buf.size)
            half = 0
        buf(first) = v            
        }

    val scale = 1.0
    val limit = 0.3
    
    def error : Double =
        {
        var err = (buf(first).abs - buf(last).abs) * buf(half).abs * scale
        if (err > limit)
            err = limit
        else if (err < -limit)
            err = -limit
        err        
        }

    def po(msg: String) =
        {
        if (counter % 5000 == 0) println(msg)
        }

    def update(v: Complex)(f:(Complex)=>Unit) : Unit =
        {
        append(v)
        var err   = error
        po("err: " + err)

        freq  = freq  + beta * err
        phase = phase + freq + alpha * err
        if (freq > maxFreq)
            freq = maxFreq
        else if (freq < minFreq)
            freq = minFreq         
        while (phase > twopi)
            phase -= twopi
        while (phase < -twopi)
            phase += twopi            

        //po("freq: " + toHertz(freq) + " corr:" + (toHertz(freq) - frequency))
        counter += 1
        }

    def update(data: Seq[Complex])(f:(Complex)=>Unit) : Unit =
        {
        data.foreach(d=> update(d)(f))
        }
}


class MM(symbolRate: Double, sampleRate: Double)
{
    val twopi = 2.0 * math.Pi
    val radiansPerSample = twopi / sampleRate
    val samplesPerRadian = 1.0 / radiansPerSample

    def toRadians(v: Double) : Double =
        v * radiansPerSample

    def toHertz(v: Double) : Double =
        v * samplesPerRadian

    var mu = 0.0
    val muGain = 1.0
    var omega = toRadians(symbolRate) 
    var omegaMid = 1.0
    var omegaRelativeLimit = 1.0
    val omegaGain = 1.0
    
    var p0 = Complex(0.0)
    var p1 = Complex(0.0)
    var p2 = Complex(0.0)

    var c0 = Complex(0.0)
    var c1 = Complex(0.0)
    var c2 = Complex(0.0)
    
    def limit(inv: Double, lim: Double) : Double =
        {
        var v = inv
        if (v > lim)
            v = lim
        else if (v < -lim)
            v = -lim
        v
        }
    
    def slicer(v: Complex) : Complex =
        {
        v
        }

    def interpolate(v: Complex, mv: Double) : Complex =
        {
            v
        }

    def update(v: Complex) : Unit =
        {
        var ii = 0
        
        p2 = p1
        p1 = p0
        p0 = interpolate(v, mu)
        
        c2 = c1
        c1 = c0
        c0 = slicer(p0)
        
        val x = (c0 - c2) * p1.conj
        val y = (p0 - p2) * c1.conj
        val u = y - x
        val out = p0
        
        val mm = limit(u.r, 1.0)
            
        omega = omega + omegaGain * mm
        omega = omegaMid + limit(omega - omegaMid, omegaRelativeLimit)
        
        mu = mu + omega + muGain * mm
        ii += mu.toInt
        mu += math.floor(mu)
        }
    
    def update(arr: Seq[Complex]) : Unit =
        {
        arr.foreach(update)   
        }
}


