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
class PropTest extends FeatureSpec with Matchers
{

    feature("Properties can be properly created and used")
        {
        scenario("Compare input and output values")
            {
			val v = new BooleanProperty("bill", "Bill Noyes", "test boolean property")(b => info("bool:" + b))
			info("v: " + v.value)
			v.value = false
			info("v: " + v.value)
			val iv = new IntProperty("joe", "Joe Chicago", "Integer property") (i => info("ival: " + i))
			iv.value = 6
			info("iv:" + iv.value)
			val sv = new StringProperty("sis", "Sissy", "quick brown") ( s => info("sval:" + s))
			sv.value = "hello world"
			val choices = List( ("First", 123.4), ("Second", 345.6),("Third", 789.0) )
			val rv = new RadioProperty("btn", "Radio Button", choices.map(_._1), "One of the following")(idx=>info("button:"+choices(idx)))
			rv.value=2
            }
  
        }        
}

