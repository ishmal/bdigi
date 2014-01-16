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


class Rfc5170Rand(initialSeed: Long)
{
    private var seed = initialSeed
    private val MAX  = 0x7FFFFFFFL
    private val MAXD = MAX.toDouble


    def apply(maxv: Long) : Int =
        {
    	var lo = 16807L * (seed & 0xFFFF);
    	val hi = 16807L * (seed >> 16);
    	lo += (hi & 0x7FFF) << 16;
    	lo += hi >> 15;
    	if (lo > MAX)
    		lo -= MAX
    	seed = lo
        val out = (seed.toDouble * maxv.toDouble / MAXD).toInt
        out
        }
}




class LdpcStaircase(k: Int, n: Int, N1: Int, seed: Int = 5170)
{
    val MAX_K = 100 //REMOVE ME!
    
    private def degree(arr: Array[Array[Byte]], row: Int) =
        arr(row).count(b => b != 0.toByte)
       
    val pchm =
        {
        val rand = new Rfc5170Rand(seed)
        val pm = Array.ofDim[Byte](n-k, k)
        val u = Array.ofDim[Int](N1*MAX_K) /* table used to have a homogeneous 1 distrib. */
    
        /* Initialize a list of all possible choices in order to
         * guarantee a homogeneous "1" distribution */
        for (h <- N1*k-1 to 0 by -1)
            u(h) = h % (n-k)
        
        /* Initialize the matrix with N1 "1s" per column, homogeneously */
        var t = 0
        for (col <- 0 until k) 
            {
            for (h <- 0 until N1) 
                { /* add N1 "1s" */
                /* check that valid available choices remain */
                var row = t
                while (row < N1*k && pm(u(row))(col) != 0)
                    row += 1
                if (row < N1*k)
                    {
                    /* choose one index within the list of possible
                     * choices */
                    do {
                        row = t + rand(N1*k-t)
                        } while (pm(u(row))(col) != 0)
                    pm(u(row))(col) = 1
    
                    /* replace with u[t] which has never been chosen */
                    u(row) = u(t)
                    t += 1
                    } 
                else 
                    {
                    do {
                       row = rand(n-k);
                        } while (pm(row)(col) !=0 )
                    pm(row)(col) = 1
                    }
                }
            }

    
        /* Add extra bits to avoid rows with less than two "1s".
         * This is needed when the code rate is smaller than 2/(2+N1) */
        for (row <- 0 until n-k) 
            {
            while (degree(pm, row) < 2) 
                {
                var col = 0
                do {
                    col = rand(k);
                    }
                while (pm(row)(col) != 0)
                pm(row)(col) = 1
                }
            }
    
    
        
        pm(0)(k) = 1
        for (row <- 0 until n-k)
            {
            pm(row)(k+row)   = 1
            pm(row)(k+row-1) = 1
            }
            
        pm
        }
        

}


