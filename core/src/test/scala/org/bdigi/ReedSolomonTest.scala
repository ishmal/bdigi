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
class ReedSolomonTest extends FeatureSpec with Matchers
{
    feature("Reed Solomon Codec")
        {
  
        /**
          Reed-Solomon coding based on the test vectors presented in
          IEEE Std 802.16-2004
    
    
          Input vector x and output vector z taken from Sect. 8.3.3.5.1, p444
    
        x = ['D4';'BA';'A1';'12';'F2';'74';'96';'30'; ...
             '27';'D4';'88';'9C';'96';'E3';'A9';'52'; ...
             'B3';'15';'AB';'FD';'92';'53';'07';'32'; ...
             'C0';'62';'48';'F0';'19';'22';'E0';'91'; ...
             '62';'1A';'C1'];
    
        z = ['49';'31';'40';'BF';'D4';'BA';'A1';'12'; ...
             'F2';'74';'96';'30';'27';'D4';'88';'9C'; ...
             '96';'E3';'A9';'52';'B3';'15';'AB';'FD'; ...
             '92';'53';'07';'32';'C0';'62';'48';'F0'; ...
             '19';'22';'E0';'91';'62';'1A';'C1';'00'];
    
        x = hex2dec(x)'; % This is the input data from the standard
        z = hex2dec(z)'; % This how the output data should be
    
        % Note that the parity bytes are first four bytes in the vector z, namely
        % '49';'31';'40';'BF' (hex).
    
    
        %% Define Reed-Solomon coding parameters 
        m = 8; % Number of bits per symbol
        n = 2^m-1; % Length of encoded data (255 for m=8)
        k = 239; % Required length of source data (ie RS(255,239) )
        t= (n-k)/2;
    
        %% Define the field generator primitive polynomial (D^8+D^4+D^3+D^2+1)
        % in decimal form according to eqn.(67), p432.
        p = 285; 
    
        %% Form the msg vector from the input data. If the input data is shorter
        %  than required, prefix it with zeros.
        Nzeros = k-length(x)-1; % Calculate how much zero padding to add.
        xk=[zeros(1,Nzeros) x 0]; % Make source data. Prefix with zero pads if needed.
    
        %% Creat Galois array from the source data
        msg=gf(xk, m, p);
    
        % Calculate the code generator polynomial according to eqn.(66) on p432 
        % of the standard (note that the 4th argument is 0 in the function call)
        % Expanding eqn.(66) gives the following polynomial coefficients:
        % 1 59 13 104 189 68 209 30 8 163 65 41 229 98 50 36 59
        gen=rsgenpoly(n, k, p, 0);
    
        %% Encode the message
        code = rsenc(msg,n,k,gen);
        % Note that Octave does not appear to place the parity bytes where expected so some
        % sorting is required.
    
        code_new = [code(k+1:k+(t/2)) code(Nzeros+1:k)];
    
        */
    
        scenario("Test 1")
            {
            val poly = 0x011d  // x8+x4+x3+x2+1
            val rs = new ReedSolomon(255, 239, poly)  //239 data bytes, 16 parity, 8 corrections
            println(rs)
            val rawdata = Array(0xD4,0xBA,0xA1,0x12,0xF2,0x74,0x96,0x30,
                                0x27,0xD4,0x88,0x9C,0x96,0xE3,0xA9,0x52,
                                0xB3,0x15,0xAB,0xFD,0x92,0x53,0x07,0x32,
                                0xC0,0x62,0x48,0xF0,0x19,0x22,0xE0,0x91,
                                0x62,0x1A,0xC1)
    
            val encdata = Array(0x49,0x31,0x40,0xBF,0xD4,0xBA,0xA1,0x12,
                                0xF2,0x74,0x96,0x30,0x27,0xD4,0x88,0x9C,
                                0x96,0xE3,0xA9,0x52,0xB3,0x15,0xAB,0xFD,
                                0x92,0x53,0x07,0x32,0xC0,0x62,0x48,0xF0,
                                0x19,0x22,0xE0,0x91,0x62,0x1A,0xC1,0x00)
    
    
            val enc = rs.encode(rawdata, false)
            for (v <- enc)
                println("%02x".format(v))
            enc shouldEqual encdata
            val dec = rs.decode(enc)
            if (dec.isDefined)
                {
                for (v <- dec.get)
                    println(v.toHexString)
                }
            else
                {
                println("failure")
                }
            }
    
        /*
        scenario("Test 2")
            {
            val poly = 0x0187  // x8+x4+x3+x2+1
            val rs = new ReedSolomon(255, 239, poly, 112, 11)  //239 data bytes, 16 parity, 8 corrections
            println(rs)
            val str = "the quick brown fox jumped over the lazy dog"
            println("#### Before")
            val rawdata = str.getBytes.map(_.toInt)
            for (v <- rawdata)
                    println("%02x".format(v))
    
            val enc = rs.encode(rawdata)
            println("#### Encoded")
            for (v <- enc)
                println("%02x".format(v))
            enc(enc.size/2) = 0xff //insert an error
            val dec = rs.decode(enc)
            println("#### Decoded")
            if (dec.isDefined)
                {
                for (v <- dec.get)
                    {
                    println("%02x".format(v))
                    }
                println(dec.get.map(_.toChar).mkString)
                }
            else
                {
                println("failure")
                }
            }
    
        scenario("Test 3")
            {
            // bob: online calc gave x^4+x^3+1 's generator as  1,13,6,12,8,15,1
            //this code says: 12,10,12,3,9,7,1    .  how do they map?
            //val poly = 0x19  // 1,1,0,0,1
            val poly = 0x13  // 1,0,0,1,1
            val data = Array(8,6,8,1,2,4,15,9,9)
            val rs = new ReedSolomon(15, 11,poly)
            println(rs)   
            }
        
        scenario("Test 4")
            {
    
            Array(//size data   gfPoly  fcs prim
                  (   3,   2,     0x7,   1,   1),
                  (   7,   5,     0xb,   1,   1),
                  (  15,  11,    0x13,   1,   1),
                  (  31,  25,    0x25,   1,   1),
                  (  63,  55,    0x43,   1,   1),
                  ( 127, 117,    0x89,   1,   1),
                  ( 255, 223,   0x11d,   1,   1),
                  ( 255, 223,   0x187, 112,  11) // Duplicates CCSDS codec
                )
      
            }
        */

        scenario("JT65")
            {
            def c8to6(arr: Array[Int]) : Array[Int] =
                {
                val buf = scala.collection.mutable.ListBuffer[Int]()
                for (i <- 0 until arr.size by 3)
                    {
                    val b0 = arr(i  )
                    val b1 = arr(i+1)
                    val b2 = arr(i+2)
                    buf += b0 >>> 2
                    buf += ((b0 & 0x03) << 4) | ((b1 & 0xf0) >>> 4)
                    buf += ((b1 & 0x0f) << 2) | ((b2 & 0xc0) >>> 6)
                    buf += b2 & 0x3f
                    }
                buf.toArray     
                }

            def c6to8(arr: Array[Int]) : Array[Int] =
                {
                val buf = scala.collection.mutable.ListBuffer[Int]()
                var count = 0
                var out = 0
                for (i <- 0 until arr.size by 4)
                    {
                    val b0 = arr(i  )
                    val b1 = arr(i+1)
                    val b2 = arr(i+2)
                    val b3 = arr(i+3)
                    buf += ((b0       ) << 2) | ((b1 & 0x30) >>> 4)
                    buf += ((b1 & 0x0f) << 4) | ((b2 & 0x3c) >>> 2)
                    buf += ((b2 & 0x03) << 6) | ((b3       )      )
                    }
                buf.toArray     
                }
                
            val rsenc = new ReedSolomon(63, 12, 0x43)
            val message1 = Array(61, 37, 30, 28,  9, 27, 61, 58, 26,  3, 49, 16)
            val msg8 = c6to8(message1)
            val enc = rsenc.encode(msg8)
            val enc6 = c8to6(enc)
            enc6.foreach(println)
            }

        }
        
    

}

