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




class CicDecimator(inRate: Int, outRate: Int)
{

    val R = inRate.toDouble / outRate.toDouble
    val gain = math.pow(R, 3.0)
    val scale = Decimal(1.0 / gain)

    var acc = -inRate
    
    //############################################
    //#  S I N G L E
    //############################################
    
    //Z-1's for the stages
    var int1   = Decimal(0)
    var int2   = Decimal(0)
    var int3   = Decimal(0)
    var comb1  = Decimal(0)
    var comb2  = Decimal(0)
    var comb3  = Decimal(0)
    
    
    def update(v: Decimal)(out: Decimal=>Unit) =
        {
        int1 += v
        int2 += int1
        int3 += int2
        
        acc += outRate
        if (acc >= 0)
            {
            acc -= inRate
            val c1 = int3 - comb1
            comb1 = int3
            val c2 = c1 - comb2
            comb2 = c1
            val c3 = c2 - comb3
            comb3 = c2
            out(c3 * scale)
            }
        }
        
        
    //############################################
    //#  IQ
    //############################################
    

    var int1i    = Decimal(0)
    var int1q    = Decimal(0)
    var int2i    = Decimal(0)
    var int2q    = Decimal(0)
    var int3i    = Decimal(0)
    var int3q    = Decimal(0)

    var comb1i   = Decimal(0)
    var comb1q   = Decimal(0)
    var comb2i   = Decimal(0)
    var comb2q   = Decimal(0)
    var comb3i   = Decimal(0)
    var comb3q   = Decimal(0)

    def updateIq(i: Decimal, q:Decimal)(out: (Decimal,Decimal)=>Unit) =
        {
        int1i += i
        int1q += q
        int2i += int1i
        int2q += int1q
        int3i += int2i
        int3q += int2q
        
        acc += outRate
        if (acc >= 0)
            {
            acc -= inRate
            val c1i = int3i - comb1i
            comb1i = int3i
            val c1q = int3q - comb1q
            comb1q = int3q
            val c2i = c1i - comb2i
            comb2i = c1i
            val c2q = c1q - comb2q
            comb2q = c1q
            val c3i = c2i - comb3i
            comb3i = c2i
            val c3q = c2q - comb3q
            comb3q = c2q

            out(c3i*scale, c3q*scale)
            }
        }


}




class CicInterpolator(inRate: Int, outRate: Int)
{
    var acc = -inRate
    
    var comb1p  = Decimal(0)
    var comb2p  = Decimal(0)
    var int1p   = Decimal(0)
    var int2p   = Decimal(0)

    
    def update(v: Decimal)(out: Decimal=>Unit) =
        {
        val comb1 = v - comb1p
        comb1p = v
        val comb2 = comb1 - comb2p
        comb2p = comb1
        
        while (acc < 0)
            {
            acc += inRate
            val int1 = comb2 + int1p
            int1p = int1
            val int2 = int1 + int2p
            int2p = int2
            out(int2)
            }
        acc -= outRate
        }


}













