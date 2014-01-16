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



import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,Matchers}

@RunWith(classOf[JUnitRunner])
class CicTest extends FeatureSpec with Matchers
{
    val siz = 1024
    
    val sine = Array.tabulate(siz)(idx =>
        {
        val angle = 2.0 * math.Pi * idx / siz
        Decimal(math.sin(angle))
        })

    feature("Can correctly decimate incoming samples")
        {
        scenario("Compare generated to expected values")
            {
			val dec = new CicDecimator(40000, 8000)
		
			for (v <- sine)
				{
				//info("v: " + v)
				info(" "*(v.toDouble*10.0 + 10.0).toInt + "#")
				dec.update(v) ( out =>
					{
					info(" "*(out.toDouble + 10.0).toInt + "*")
					//info("     " + out)
					})
				}
            }
  
        }        
}

