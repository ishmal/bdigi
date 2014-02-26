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


/**
 * A sine generator with a 32-bit accumulator and a 16-bit
 * lookup table.  Much faster than above.
 */
class Nco(frequency: Double, sampleRate: Double)
{
    val freq  = (4294967296.0 * frequency / sampleRate).toLong
    var phase = 0L
    
    def next : Complex =
        {
        phase += freq
        Nco.table((phase >> 16).toInt & 0xffff)
        }
        
}



object Nco
{
    private val twopi = math.Pi * 2.0
    private val two16 = 1 << 16
    private val delta = twopi / (two16 .toDouble)
    
    val table = Array.tabulate(two16) ( idx =>
        {
        val angle = delta * idx.toDouble
        Complex( math.cos(angle), math.sin(angle) ) 
        })

}




