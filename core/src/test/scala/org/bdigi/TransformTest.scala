/**
 * Scala SDR tool
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2013 Bob Jamison
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
class TransformTest extends FeatureSpec with Matchers
{
        
    feature("Look at W values for DFT")
        {
  
        scenario("Test 2")
            {
            val sampleRate = 10000.0
            val N          = 1000
            val bins       = 300
            val lowFreq    = 0.0
            val highFreq   = 3000.0
            val t1         = new SlidingRealDft2(N, bins)
            val t2         = SimpleDft(N, lowFreq, highFreq, sampleRate)

            for (i <- 0 until bins)
                {
                val w1 = Complex(t1.Wr(i),      t1.Wi(i))
                val w2 = Complex(t2.bins(i).wr, t2.bins(i).wi)
                //info("w1: " + w1)
                //info("w2: " + w2)
                assert(w1.equals(w2, 0.001), "i:" + i + " w1:" + w1 + " did not equal w2:" + w2)
                }
                
            info("impulse test")
            t1.update(1.0)
            t2.update(1.0)
            for (i <- 0 until 1000)
                {
                t1.update(0.0)
                t2.update(0.0)
                val v1 = t1.X(bins-1)
                val v2 = t2.X(bins-1)
                //info("v1: " + v1)
                //info("v2: " + v2)
                assert(v1.equals(v2, 0.001), "i:" + i + " v1:" + v1 + " did not equal v2:" + v2)
                val ps1 = t1.powerSpectrum
                val ps2 = t2.powerSpectrum
                for (j <- 0 until bins)
                    {
                    val p1 = ps1(j)
                    val p2 = ps2(j)
                    assert((math.abs(p1-p2) < 0.001), "j:" + i + " p1:" + p1 + " did not equal p2:" + p2)
                    }
                }
            }
    
            
        }
        
}

