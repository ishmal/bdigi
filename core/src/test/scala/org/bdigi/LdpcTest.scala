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
class CRC32Test extends FeatureSpec with Matchers
{
    feature("CRC32")
        {
        scenario("Fox test")
            {
            val res = CRC32("the quick brown fox".getBytes)
            val exp = 0x91C102CAL
            info("res:" + res + " exp:" + exp)
            res should equal (exp)
            }
        }
    

}


@RunWith(classOf[JUnitRunner])
class TannerTest extends FeatureSpec with Matchers
{
    feature("Tanner Tree")
        {
        scenario("Generating a Tanner tree")
            {
            val H = ByteMatrix(6,
                1, 0, 0, 1, 0, 1,
                0, 1, 0, 0, 1, 0,
                1, 0, 1, 0, 0, 1        
                )
            
            info("H:" + H)
    
            val t = new Tanner(H)
            info("=== Variable nodes === :" + t.v.size)
            for (n <- t.v) info(n.toString)
            info("=== Check nodes === :" + t.c.size)
            for (n <- t.c) info(n.toString)
                
            }
        }
    

}


@RunWith(classOf[JUnitRunner])
class Rfc5170RandTest extends FeatureSpec with Matchers
{
    feature("Rfc5170Rand")
        {
        scenario("Make sure that the 10000th value is what is expected")
            {
			var v = 0L
			var prng = new Rfc5170Rand(1)
			for (i <- 0 until 10000)
				v = prng(0x7FFFFFFFL)
			v shouldEqual 1043618065L
			}
        }
    

}


@RunWith(classOf[JUnitRunner])
class BitSetTest  extends FeatureSpec with Matchers
{

    feature("BitSet")
        {

        scenario("to Bits")
            {
            val bytes = Array(0xaa.toByte, 0x55.toByte)
            val bits = Bits.bytesToBits(bytes)
            val exp = Array(1.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte,
                            0.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte)
            if (bits.toList != exp.toList)
                {
                info("lists not equal")
                info("bits:")
                bits.foreach(print)
                info("\nexpected:")
                exp.foreach(print)
                }
            bits.toList should equal (exp.toList)
            }
    
        scenario("to Bits And Back")
            {
            val bytes = "the quick brown fox".getBytes
            val bits  = Bits.bytesToBits(bytes)
            val res   = Bits.bitsToBytes(bits)
            if (bytes.toList != res.toList)
                {
                info("lists not equal")
                info("bytes:" + bytes.mkString("[", ",", "]"))
                info("res  :" + res.mkString("[", ",", "]"))
                }
            bytes.toList should equal (res.toList)
            }
    
        scenario("shift test")
            {
            val bits = Array[Byte](1, 1, 1, 1, 0, 0, 0, 0)
            val set  = new Bits(bits)
            val res  = (set >> 5).bits
            val exp  = Array[Byte](1, 0, 0, 0, 0, 1, 1, 1)
            if (res.toList != res.toList)
                {
                info("lists not equal")
                info("res :" + res.mkString("[", ",", "]"))
                info("exp :" + exp.mkString("[", ",", "]"))
                }
            res.toList should equal (exp.toList)
            }
        
        scenario("xor test")
            {
            val bits1 = Array[Byte](1, 1, 1, 1, 0, 0, 0, 0)
            val set1  = new Bits(bits1)
            val bits2 = Array[Byte](1, 0, 0, 0, 0, 1, 1, 1)
            val set2  = new Bits(bits2)
            val res  =  (set1 ^ set2).bits
            val exp  = Array[Byte](0, 1, 1, 1, 0, 1, 1, 1)
            if (res.toList != exp.toList)
                {
                info("lists not equal")
                info("res :" + res.mkString("[", ",", "]"))
                info("exp :" + exp.mkString("[", ",", "]"))
                }
            res.toList should equal (exp.toList)
            }

        }    
}


@RunWith(classOf[JUnitRunner])
class LdpcTest extends FeatureSpec with Matchers
{
    feature("LDPC")
        {

        scenario("encoding a big string of 'a'")
            {
            val aa = "a" * 40
            val encoded = Ldpc.code0648_27_1_2.encode(aa.getBytes)
            }
       
        scenario("encoding and decoding")
            {
            val plain = for (i <- 0 until 648) yield (i & 0x01).toByte
            val origCrc = CRC32(plain)
            val code = Ldpc.code0648_27_1_2
            val encoded = code.encode(plain.toArray)
            val decoded = code.decode(encoded)
            val finalCrc = CRC32(plain)
            if (origCrc == finalCrc)
                info("Test passed")
            else
                info("Test failed")
            }

        }//feature LDPC
}
