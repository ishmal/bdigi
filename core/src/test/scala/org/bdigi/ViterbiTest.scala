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
class ViterbiTest extends FeatureSpec with Matchers
{
    feature("Convolutional encoding and Viterbi decoding")
        {
  
        scenario("Test 1")
            {
            val k = 3
            val poly1 = 7
            val poly2 = 5
            //010111001010001
            val inp = List(false, true, false, true, true, true, false, false,
                true, false, true, false, false, false, true)
            //val inp = List(true, false, false)
            val encoder = Viterbi.encoder(k, poly1, poly2)
            println(encoder)
            val enc = encoder.encodeBits(inp).map(Viterbi.toBits)
            println("in  : " + inp.mkString(", "))
            println("enc : " + enc.mkString(", "))
            }
    
        scenario("Test 2")
            {
            val k = 3
            val poly1 = 7
            val poly2 = 5
            val encoder = Viterbi.encoder(k, poly1, poly2)
            val decoder = Viterbi.decoder(k, poly1, poly2)
            println("=== decoder:\n" + decoder)
            val inp = List(123,0,0,0,0,0,0,0)
            val enc = encoder.encodeWords(inp, 8)
            println("in  : " + inp)
            println("enc : " + enc.map(Viterbi.toBits).mkString(", "))
            val dec = Viterbi.fromBits(decoder.decodeHard(enc))
            println("dec: " + dec.mkString(","))
            }
    
        scenario("Test 3")
            {
            val k = 5
            val poly1 = 0x17
            val poly2 = 0x19
            val encoder = Viterbi.encoder(k, poly1, poly2)
            val decoder = Viterbi.decoder(k, poly1, poly2)
            println(encoder)
            val inp = "the quick brown fox jumped over the lazy dog's back"
            val enc = encoder.encodeStr(inp + "       ").toArray
            println("encoded: " + enc.map(Viterbi.toBits).mkString(","))
            enc(5) = 0  
            //println("broken: " + enc.map(Viterbi.toBits).mkString(","))  
            val dec = Viterbi.fromBits(decoder.decodeHard(enc))
            val str = dec.map(_.toChar).mkString.substring(7)
            println("dec: '" + str + "'")
            if (inp == str)
                println("success")
            else
                println("failure")
            }
    
        scenario("Test 4")
            {
            val k = 5
            val poly1 = 0x17
            val poly2 = 0x19
            val encoder = Viterbi.encoder(k, poly1, poly2)
            val decoder = Viterbi.decoder(k, poly1, poly2)
            val inp = "the quick brown fox jumped over the lazy dog's back"
            var i = 0
            var keepGoing = true
            while (keepGoing && i < 20000)
                {
                val enc = encoder.encodeStr(inp + "       ").toArray
                enc(5) = 0  
                val dec = Viterbi.fromBits(decoder.decodeHard(enc))
                val str = dec.map(_.toChar).mkString.substring(7)
                if (str != inp)
                    {
                    println("fail at " + i + " : '" + str + "'")
                    keepGoing = false
                    }
                if (i % 10000 == 0) println(i)
                i += 1
                }
            }

        }
    

}

