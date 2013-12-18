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

import org.bdigi.mode.{Psk31,QuadCodec,Varicode}


@RunWith(classOf[JUnitRunner])
class Psk31Test extends FeatureSpec with Matchers
{

    var bval = 0.0
    def encodeBpsk(c: Int) : Seq[Complex] =
        {
        val bits = Varicode.encodeTable(c & 127) ++ Array(false, false) 
        //info("c: " + c.toChar + " : " + bits.mkString(", "))
        bits.map(bit =>
            {
            if (!bit) bval = -bval
            Complex(bval, 0.0)
            })
        }
    
    def encodeBpsk(str: String) : Seq[Complex] =
        {
        val ascii = str.getBytes.map(_.toInt)
        val buf = scala.collection.mutable.ListBuffer[Complex]()
        for (a <- ascii)
            buf ++= encodeBpsk(a)
        buf.toSeq
        }
    
    val encoder = Viterbi.encoder(5, 0x17, 0x19)
    val decoder = Viterbi.decoder(5, 0x17, 0x19)
    
    var lastqval = Complex(1.0)

    def encodeQpsk(c: Int) : Seq[Complex] =
        {
        val bits = Varicode.encodeTable(c & 127) ++ Array(false, false) 
        val dibits = encoder.encodeBits(bits)
        //info("c: " + c.toChar + " : " + bits.mkString(", "))
        dibits.map(dibit =>
            {
            val qval = if (dibit == 0)
                {
                Complex(-lastqval.r, -lastqval.i)  //opposite
                }
            else if (dibit == 1)  
                {
                Complex(-lastqval.i, lastqval.r)    //90 deg counterclockwise
                }
            else if (dibit == 2)
                {
                Complex(lastqval.i, -lastqval.r)  //90 deg clockwise
                }
            else
                {
                lastqval  //dont move
                }
            info("out: %d %s %s".format(dibit, lastqval, qval))
            lastqval = qval
            qval
            })
        }
    
    def encodeQpsk(str: String) : Seq[Complex] =
        {
        val ascii = str.getBytes.map(_.toInt)
        val buf = scala.collection.mutable.ListBuffer[Complex]()
        for (a <- ascii)
            buf ++= encodeQpsk(a)
        buf.toSeq
        }








    feature("PSK31 - specific functionality")
        {
  
        scenario("Varicode tables")
            {
            Varicode.printTables
            }

    
        scenario("Test 1")
            {
            val app = new App
                {
                override def puttext(v: String) = { print(v) }
                }
            val psk = new Psk31(app)
            encodeBpsk(0)
            var str = "the quick brown fox\n"
            var cpx = encodeBpsk(str)
            for (v <- cpx)
                {
                //info("v: " + v)
                psk.processSymbol(v)
                }
            cpx = encodeBpsk(str)
            for (v <- cpx)
                {
                //info("v: " + v)
                psk.processSymbol(v)
                }
            app.stop
            }
        
        scenario("Test 2")
            {
            val qc = new QuadCodec
            val inp = List(0,1,2,3,0,1,2,3)
            info("inp: " + inp.mkString(","))
            val cpx = qc.encode(inp)
            for (v <- cpx)
                {
                val out = qc.toIndex(qc.decode(v))
                info("out: " + out)
                }
            }
        
        scenario("Test 3")
            {
            val app = new App
                {
                override def puttext(v: String) = { print(v) }
                }
            val psk = new Psk31(app)
            psk.qpskMode = true
            encodeQpsk(0)
            val str = "ryryryryryryryryryryryryry"
            //val str = "the quick brown fox"
            val cpx = encodeQpsk(str)
            for (v <- cpx)
                {
                //info("v: " + v)
                psk.processSymbol(v)
                }
            app.stop
            }

        }
    

}

