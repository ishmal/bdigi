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
class NcoTest extends FeatureSpec with Matchers
{
    feature("Generates a valid stream of values")
        {
        scenario("Compare generated to expected values")
            {
            val freq = 87.0
            val fs = 1000.0
            val twopi = 2.0 * math.Pi
            val nco = new Nco(freq, fs)
            val pfreq = twopi * freq / fs
            var phaseacc = 0.0
            for (i <- 0 until 1000)
                {
                phaseacc += pfreq
                if (phaseacc >= twopi)
                    phaseacc -= twopi
                val exp = math.cos(phaseacc)
                val v = nco.next.r
                //info("v:" + v + "  exp:" + exp)
                math.abs(v - exp) should be < 0.001
                }
            }
  
        }        
}

