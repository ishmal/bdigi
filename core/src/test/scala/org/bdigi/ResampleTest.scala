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
class ResampleTest extends FeatureSpec with Matchers
{
    feature("FirDecimator")
        {

        scenario("Proper scaling")
            {
            val resampler = new FirResampler(5)
            val sample = 5.0
            for (i <- 0 until 100) //preload
                resampler.decimate(sample) (v=>{})
            for (i <- 0 until 100)
                {
                resampler.decimate(sample) ( v =>
                    {
                    //info("dec: " + v)
                    assert (math.abs(sample - v) < 0.01)
                    })
                }
            }
    

        }//feature FirDecimator

    feature("FirInterpolator")
        {

        scenario("Proper scaling")
            {
            val resampler = new FirResampler(6)
            val sample = 5.0
            for (i <- 0 until 100) //preload
                resampler.interpolate(sample) (v=>{})
            for (i <- 0 until 100)
                {
                resampler.interpolate(sample) ( v =>
                    {
                    //info("int: " + v)
                    assert (math.abs(sample - v) < 0.01)
                    })
                }
            }
    

        }//feature FirInterpolator
}
