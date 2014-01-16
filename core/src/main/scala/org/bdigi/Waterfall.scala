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




/**
 * This is a GUI-less waterfall that can be used for various purposes
 */
class Waterfall(width: Int, length: Int, N: Int, sampleRate: Double, maxFreq: Double)
{

        private val frame = Array.fill(N)(0.0)
        private val bins = (1.0 * maxFreq / sampleRate * N).toInt
        Log.trace("wf samplerate: " + sampleRate + "  bins:" + bins)
        private val window = Window.Hamming(N)
    
        private val wf = Array.ofDim[Int](width, length)
        private var wfptr = 0     
            
        /**
         * Make a palette. tweak this often
         */                 
        private val colors = Array.tabulate(256)( i=>
            {
            val r = if (i < 170) 0 else (i-170) * 3
            val g = if (i <  85) 0 else if (i < 170) (i-85) * 3 else 255
            val b = if (i <  85) i * 3 else 255
            var col = 0xff
            col = (col << 8) + r
            col = (col << 8) + g
            col = (col << 8) + b
            col
            })
            
        /*Scale the power spectrum bins onto the output width.  Do once & reuse. */
        private val psIndices = Array.tabulate(width)(_ * bins / width)
                
            
        private def renderToBuffer(ps: Array[Double]) =
            {
            val pslen = ps.size
            val row = wf(wfptr)
            wfptr = (wfptr + 1) % length

            for (i <- 0 until width)
                {
                val p = ps(psIndices(i))
				val v = MathUtil.log1p(p) * 20.0
				val colidx = v.toInt & 0xff
				val col = colors(colidx)
                row(i) = colors(colidx)
                }
            }
        
        
        private val trans = new DFft(N)
        
        private var framePtr = 0
        private var frameCtr = 0
        private val SUBN = N/5
        
        private val slidingbuf = Array.ofDim[Double](N)
        
        def update(v: Double) =
            {
            frame(framePtr) = v
            framePtr = (framePtr + 1) % N
            frameCtr += 1
            if (frameCtr >= SUBN)
                {
                frameCtr = 0
                var fp = (framePtr + 1) % N
                for (i <- 0 until N)
                    {
                    slidingbuf(i) = frame(fp)  * window(i)
                    fp = (fp + 1) % N
                    }
                val ps = trans.powerSpectrum(slidingbuf, bins)
                renderToBuffer(ps)
                }
            }
            
    
}

