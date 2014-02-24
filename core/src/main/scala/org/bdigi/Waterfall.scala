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
class WaterfallFactory(par: App, N: Int, sampleRate: Double, maxFreq: Double)
{
    private val frame = Array.fill(N)(0.0)
    private val bins = (maxFreq / sampleRate * N).toInt
    par.trace("wf samplerate: " + sampleRate + "  bins:" + bins)
    private val window = Window.Hamming(N)

    private val length = 5
    private val wf = Array.ofDim[Int](length, bins)
    private var wfptr = 0     
        
    /**
     * Make a palette. tweak this often
     */                 
    private val colors = Array.tabulate(256)( i =>
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
        
    private val logTable = Array.tabulate(65536) ( i =>
        {
        (math.log(i+1) * 23.0).toInt
        })
                      
    private val trans = new DFft(N)
    
    private var framePtr = 0
    private var frameCtr = 0
    private val SUBN = N/5
    
    private val slidingbuf = Array.ofDim[Double](N)
    
    def update(v: Double)(f: (Array[Int]) => Unit) =
        {
        frame(framePtr) = v
        framePtr = (framePtr + 1) % N
        frameCtr += 1
        if (frameCtr >= SUBN)
            {
            frameCtr = 0
            //var fp = (framePtr + 1) % N
            var fp = framePtr
            for (i <- 0 until N)
                {
                slidingbuf(i) = frame(fp)  *  window(i)
                fp = (fp + 1) % N
                }
            val row = wf(wfptr)
            wfptr = (wfptr + 1) % length
            val ps = trans.intPowerSpectrum(slidingbuf, row)
            f(row)
            }
        }   
}

