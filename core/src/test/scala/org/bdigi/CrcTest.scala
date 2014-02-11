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

package org.bdigi.mode

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,Matchers}


@RunWith(classOf[JUnitRunner])
class CrcTest extends FeatureSpec with Matchers
{
    def vout(v: Int)
        {
        info("val: %04x".format(v))
        }
        
    feature("Correctness")
        {
        scenario("Null length message")
            {
            val crc = new CrcCCITT
            val v = crc.value
            vout(v)
            v shouldEqual 0xffff
            }
        scenario("Single letter 'A'")
            {
            val crc = new CrcCCITT
            crc.update('A'.toInt)
            val v = crc.value
            vout(v)
            v shouldEqual 0xb915
            }
        scenario("Calculates '123456789'")
            {
            val arr = "123456789".getBytes.map(_.toInt)
            val crc = new CrcCCITT
            for (b <- arr) crc.update(b)
            val v = crc.value
            vout(v)
            v shouldEqual 0x29b1
            }
        scenario("the quick brown fox")
            {
            val arr = "the quick brown fox".getBytes.map(_.toInt)
            val crc = new CrcCCITT
            for (b <- arr) crc.update(b)
            val v = crc.value
            vout(v)
            v shouldEqual 0x7e06
            }
        scenario("Calculates 256 'A'")
            {
            val arr = Array.fill(256)('A'.toInt)
            val crc = new CrcCCITT
            for (b <- arr) crc.update(b)
            val v = crc.value
            vout(v)
            v shouldEqual 0xea0b
            }
        }

}


