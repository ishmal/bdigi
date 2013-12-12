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
class MathTest extends FeatureSpec with Matchers
{
    feature("Check our version of log()")
        {
  
        scenario("Compare fast log to native log")
            {
            var v = 0.1
            while (v < 100.0)
                {
                info("v : " + v)
                info("math.log     : " + math.log(v))
                info("MathUtil.log : " + MathUtil.log(v))
                v += 0.1
                }
            }
    
        scenario("Compare fast and faster log")
            {
            var v = 0.1
            while (v < 100.0)
                {
                info("v : " + v)
                info("math.log    : " + MathUtil.log(v))
                info("math.log2   : " + MathUtil.log2(v.toFloat))
                v += 0.1
                }
            }
    
        scenario("Compare fast and faster log times")
            {
            val start1 = System.nanoTime
            for (i <- 0 until 100000000)
                {
                MathUtil.log(1000.0)
                }
            val end1 = System.nanoTime
            info("t1: " + (end1 - start1))
            val start2 = System.nanoTime
            for (i <- 0 until 100000000)
                {
                MathUtil.log2(1000.0f)
                }
            val end2 = System.nanoTime
            info("t2: " + (end2 - start2))
            }
    
        }
        
    feature("Decimal numbers")
        {
        scenario("Create and use decimals")
            {
			val a = Decimal(4.5)
			val b = Decimal(5.6)
			val c = Decimal(-3.8)
			info("%s * %s = %s".format(a, b, a*b))
			info("%s * %s = %s".format(a, c, a*c))
			info("%s * %s = %s".format(c, c, c*c))
            }
        }
        
         
}

