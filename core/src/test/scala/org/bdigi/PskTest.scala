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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,Matchers}

@RunWith(classOf[JUnitRunner])
class PskTest extends FeatureSpec with Matchers
{
    feature("PSK Common Functionality")
        {
  
        scenario("Test 1")
            {
            val frequency  = 1000.0
            val bandwidth  = 50.0
            val sampleRate = 11025.0
            
            val nco = new Nco(frequency, sampleRate)            
            
            val loop = new CostasLoop2(frequency - 10.0, bandwidth, sampleRate)
            
            for (i <- 0 until 1000)
                {
                val d = nco.next
                loop.update(d.i)
                }
            }
    
        scenario("Test 2")
            {
            val frequency  = 1000.0
            val sampleRate = 11025.0
            
            val nco = new Nco(frequency, sampleRate)            
            
            val loop = new GardnerLoop2(frequency, sampleRate)
            
            for (i <- 0 until 1000)
                {
                val s = Complex(nco.next.r)
                loop.update(s)(d =>
                    {
                    println("out: " + d)
                    })
                }
            }

        }
    

}

