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

import sdr.mode.{PacketAddr}

@RunWith(classOf[JUnitRunner])
class PacketTest extends FeatureSpec with Matchers
{
    feature("Packet Common Functionality")
        {
  
        scenario("Address encoding test")
            {
            val addr = PacketAddr("WB4JFI", 1)
            val res = addr.encoded
            println("res: ")
            for (b <-res) println("%02X ".format(b))
            val exp = Array(0xae, 0x84, 0x68, 0x94, 0x8c, 0x92, 0x62).map(_.toByte)
            println("exp: ")
            for (b <-exp) println("%02X ".format(b))
            res shouldEqual exp
            }
    

        }
    

}

