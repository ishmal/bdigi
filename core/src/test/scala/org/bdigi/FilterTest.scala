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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,Matchers}

@RunWith(classOf[JUnitRunner])
class FilterTest extends FeatureSpec with Matchers
{
    feature("FIR Filters")
        {

        scenario("Proper coefficients")
            {
            //Basic Low Pass coefficients
            val f = Fir.lowPass(21, 460.0, 2000.0, Window.Rectangle)
            for (c <- f.coeffs)
                info(c.toString)            
            //Basic Raised Cosine coefficients
            //note, they suggest size of >= 4*samplesPerSymbol + 1
            val f2 = Fir.raisedCosine(21, 0.5, 50.0, 100.0, Window.Rectangle)
            for (c <- f2.coeffs)
                info(c.toString)            
            }
    
        def f2Test(f: Fir) =
            {
            val size = 100
            val fft = new Fft
            val acc = Array.fill(size)(Complex(0.0))
            for (i <- 0 until 100)
                {
                val data = Array.fill(size)(Complex(math.random))
                val res = data.map(f.update)
                val timeData = fft.forward(res.toArray)
                for (i <- 0 until res.size)
                    acc(i) += timeData(i)
                }            
            for (i <- 0 until acc.size)
                info("i:" + i + " : " + acc(i).mag)
            }
    
        scenario("Filter tests")
            {
            val size = 100
            val sampleRate = 100.0
            
            //===== LowPass ======"
            val f1 = Fir.lowPass(17, 25.0, sampleRate)
            f2Test(f1)
    
            //===== HighPass ======
            val f2 = Fir.highPass(17, 25.0, sampleRate)
            f2Test(f2)
    
            //===== BandPass ======
            val f3 = Fir.bandPass(17, 20.0, 30.0, sampleRate)
            f2Test(f3)
    
            //===== BandStop ======
            val f4 = Fir.bandStop(17, 20.0, 30.0, sampleRate)
            f2Test(f4)
    
            }
        
        scenario("Impulse test")
            {
            val f = Fir.lowPass(21, 1000.0, 10000.0)
            val v = f.update(1.0)
            info("0: " + v)
            info("c0: " + f.coeffs(0))
            for (i <- 1 until 21)
                {
                val b = f.update(0)
                info("" + i + ": " + b)
                info("c" + i + ": " + f.coeffs(i))
                }
            }

        }//feature FIR
}
