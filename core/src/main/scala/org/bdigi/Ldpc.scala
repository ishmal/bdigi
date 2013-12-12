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


/**
 * This is a simple wrapper to enable easy use of the Java runtime's CRC-32 class
 */
object CRC32
{
    /**
     * Returns the 32-bit value of the current CRC.  It is returned in a Long
     * in order to avoid signedness.
     */              
    def apply(bytes : Seq[Byte]) : Long =
        {
        val engine = new java.util.zip.CRC32
        engine.update(bytes.toArray)
        engine.getValue
        }

    /**
     * Returns the 32-bit CRC of the given byte array in the form of a big-endian
     * array of bytes
     */
    def bigEndian(bytes : Seq[Byte]) : Seq[Byte] =
        {
        val lval = apply(bytes)
        val v  = Seq(((lval >> 24) & 0xff).toByte, ((lval >> 16) & 0xff).toByte,
                     ((lval >>  8) & 0xff).toByte, ((lval      ) & 0xff).toByte)
        v
        }

    /**
     * Returns the 32-bit CRC of the given byte array in the form of a little-endian
     * array of bytes
     */
    def littleEndian(bytes : Seq[Byte]) : Seq[Byte] =
        {
        val lval = apply(bytes)
        val v  = Seq(((lval      ) & 0xff).toByte, ((lval >>  8) & 0xff).toByte,
                     ((lval >> 16) & 0xff).toByte, ((lval >> 24) & 0xff).toByte)
        v
        }
}





/**
 * Creates a Tanner diagram that can be used as a decoder
 * @see http://en.wikipedia.org/wiki/Tanner_graph
 * 
 */ 
class Tanner(H: ByteMatrix)
{
    trait Node
        {
        val index : Int
        val nodebuf = scala.collection.mutable.ListBuffer[Link]()
        lazy val nodes = nodebuf.toArray            
        }
    class VNode(val index: Int = 0) extends Node
        {            
        override def toString : String =
            {
            val childstr = nodes.map(_.c.index).mkString("[", ",", "]")
            "node(" + index + ") " + childstr
            }
        }
    class CNode(val index: Int = 0) extends Node
        {            
        override def toString : String =
            {
            val childstr = nodes.map(_.v.index).mkString("[", ",", "]")
            "node(" + index + ") " + childstr
            }
        }
    
    case class Link(v: VNode, c: CNode)
        {
        //probabilities from v nodes to c nodes
        var v0 = 0.0
        var v1 = 0.0
        //probabilities from c nodes to v nodes
        var c0 = 0.0
        var c1 = 0.0
        }
        
    val rows = H.rows
    val cols = H.cols

    //First make the nodes so we have something to point to
    val v = Array.ofDim[VNode](cols)
    for (i <- 0 until cols)
        v(i) = new VNode(i)
    val c = Array.ofDim[CNode](rows)
    for (i <- 0 until rows)
        c(i) = new CNode(i)

    //Now for each 1 in H at the intersection of row and column,
    //put a Link between them
    for (row <- 0 until rows ; col <- 0 until cols)
        {
        if (H(row, col) != 0) 
            {
            val vn = v(col)
            val cn = c(row)
            val link = Link(vn, cn)
            vn.nodebuf += link
            cn.nodebuf += link
            }
        }
        
    def decode(info: Array[Double], iters: Int) : Array[Double] =
        {
        if (info.size != cols)
            {
            throw new IllegalArgumentException("data size:" + info.size + " does not match code size: " + cols)
            }

        //Initial load.  Nothing else is known
        for (i <- 0 until cols)
            {
            val n = v(i)
            for (link <- n.nodes)
                {
                link.v1 = info(i)
                link.v0 = 1.0 - info(i)
                }
            }
        
        //Do our thinking
        for (iter <- 0 until iters)
            {
            /*
            //from variable nodes to check nodes
            for (cn <- c)
                {
                var p1sum = 0.0
                var p0sum = 0.0
                val scale = cn.nodes.size.toDouble
                for (n <- cn.nodes)
                    {
                    prsum += n.p1
                    pisum += n.p0
                    }
                cn.p1 = p1sum / scale
                cn.p0 = p0sum / scale
                }                

            //from check nodes to variable nodes
            for (vn <- v)
                {
                var p1sum = 0.0
                var p0sum = 0.0
                val scale = vn.nodes.size.toDouble
                for (n <- vn.nodes)
                    {
                    p1sum += n.p1
                    p0sum += n.p0
                    }
                vn.p1 = prsum / scale
                vn.p0 = pisum / scale
                }
            */                
            }

        //Copy out the result
        val res = Array.ofDim[Double](cols)        
        //for (i <- 0 until w)
        //    res(i) = v(i).pr
        res
        }

}


/**
 * Simple utility class for handling blocks of bits, such as the
 * 27-,54- and 81-bit block sizes of the qc-ldpc matrices here.
 * 
 * Name was originally BitSet, but it did not really have a bitset model.
 */
class Bits(inputBits: Array[Byte])
{
    outer =>
    
    val bits = inputBits.clone
    
    def >>(places: Int) : Bits =
        {
        if (places == 0)
            outer
        else
            {
            val newarr = bits.takeRight(places) ++ bits.take(bits.size - places)
            new Bits(newarr)
            }
        }

    def ^(other: Bits) : Bits =
        {
        val newarr = for (i <- 0 until bits.size) yield ((bits(i) + other.bits(i)) & 0x1).toByte
        new Bits(newarr.toArray)
        }

}


object Bits
{
    def apply(size: Int) : Bits =
        {
        val arr = Array.ofDim[Byte](size)
        new Bits(arr)
        }

    def apply(byteArr: Array[Byte], size: Int) : Array[Bits] =
        {
        val bits = fromBitArray(bytesToBits(byteArr), size)
        bits
        }

    def fromBitArray(bitarr: Array[Byte], size: Int) : Array[Bits] =
        {
        val bits = bitarr.grouped(size).map(b=> new Bits(b)).toArray
        bits
        }
    
    /**
     * Convert an array of bytes to an array of bits
     */
    def bytesToBits(arr: Array[Byte]) : Array[Byte] =
        {
        val out = Array.ofDim[Byte](arr.size * 8)
        var outidx = 0
        for (byteval <- arr)
            {
            var b = byteval.toInt
            for (i <- 0 until 8)
                {
                out(outidx) = if ((b & 128) != 0) 1 else 0
                b <<= 1
                outidx += 1
                }
            }
        out
        }

    /**
     * Convert an array of bits to an array of bytes
     */         
    def bitsToBytes(bitarr: Array[Byte]) : Array[Byte] =
        {
        val out = scala.collection.mutable.ListBuffer[Byte]()
        var byteval = 0
        var bitmask = 128
        for (bitval <- bitarr)
            {
            if (bitval != 0)
                byteval |= bitmask
            bitmask >>= 1
            if (bitmask <= 0)
                {
                out += byteval.toByte
                byteval = 0
                bitmask = 128
                }
            }
        if (bitmask < 128)
            out += byteval.toByte
        out.toArray
        }
    
    def stringToBits(str: String) : Array[Byte] =
        bytesToBits(str.getBytes)
}


/**
 * Simple array wrapper to hold short ints
 */
class ByteMatrix(rowp: Int, colp: Int, arr: Array[Array[Byte]])
{
    val rows = rowp
    val cols = colp
    
    def apply(row: Int, col: Int) : Int =
        arr(row)(col).toInt

    def update(row: Int, col: Int, v: Int)  =
        arr(row)(col) = v.toByte
        
    override def toString =
        arr.map(_.mkString(",")).mkString("{\n", ",\n", "\n}")
}

object ByteMatrix
{
    def apply(rows: Int, cols: Int) : ByteMatrix =
        {
        new ByteMatrix(rows, cols, Array.ofDim[Byte](rows, cols))
        }

    def apply(cols: Int, vals: Int *) : ByteMatrix =
        {
        val rows = vals.size / cols
        val m = apply(rows, cols)
        var i = 0
        for (r <- 0 until rows ; c <- 0 until cols)
            {
            m(r, c) = vals(i)
            i += 1
            }
        m
        }
}


/**
 * Creates an LDPC code matrix
 */
case class Code(qc: ByteMatrix, blockSize: Int, infoSize: Int, zSize: Int)
{
    val hcols = qc.cols * zSize
    val hrows = qc.rows * zSize
    
    /**
     * Generate the full-size matrix from the QC definition
     */         
    val H =
        {  
        //qrow and qcol are rows and columns in the input QC matrix
        //hrow and hcol are rows and columns in the output LDPC matrix
        val newh = ByteMatrix(hrows, hcols)
        var hrow = 0
        var hcol = 0
        for (qrow <- 0 until qc.rows; qcol <- 0 until qc.cols)
            {
            var shift = qc(qrow, qcol)
            if (shift >= 0)
                {
                for (i <- 0 until zSize)
                    {
                    newh(hrow + i, hcol + shift) = 1
                    shift = (shift + 1) % zSize
                    }  
                }
            hcol += zSize
            if (hcol >= hcols)
                {
                hcol = 0;
                hrow += zSize
                }
            }
        newh
        }

    val graph = new Tanner(H)


    
    /*
    def validate(bitArr: Array[Byte]) : Boolean =
        {
        val bsize = bitArr.size
        if (hcols != bsize)
            {
            println("array size:" + bsize + " does not equal PCM row size:" + hcols)
            false
            }
        !H.exists(row =>
            {
            var tot = 0
            for (i <- 0 to hcols) tot != row(i) * bitArr(i)
            tot > 0
            })    
        }
    */

    /**
     * Divide the parity check matrix H into a left
     * matrix H1 = #databits x m  and right H2 = #checkbits x m .
     * The first column of H2 is hT. 
     *          
     * First make an array R(i) of blocks which is the result of H1(i) * sT
     * p0T = sum(R(i))
     * p1T = R(1) + h0 * p0T
     * p2T = R(2) + h1 * p0T + p1T
     * .
     * .
     * p(m-1)T = R(m-1) + h(m-2) * p0T + p(m-2)T 
     *       
     */        
    def encode(bitArr: Array[Byte]) : Array[Byte] =
        {
        val dataSize = bitArr.size
        if (dataSize > infoSize)
            {
            throw new IllegalArgumentException("Incorrect size: " + dataSize + "  Expected: " + infoSize)
            }
        val dataBitArr = 
            {
            if (dataSize == infoSize)
                bitArr
            else
                bitArr ++ Array.ofDim[Byte](infoSize - dataSize)
            }
        val m = blockSize - infoSize
        val dataBits = Bits.fromBitArray(dataBitArr, zSize)
        val R = Array.tabulate(qc.rows) (row =>
            {
            var rowttl = Bits(zSize)
            for (i <- 0 until infoSize)
                {
                val shift = qc(row, i)
                if (shift >= 0)
                    rowttl = rowttl ^ (dataBits(i) >> shift)
                }
            rowttl
            })
        val p0 = R.foldLeft(Bits(zSize))(_ ^ _)
        println(p0)

        val parityBits = Array.ofDim[Bits](m)
        parityBits(0) = p0
        var prev = Bits(zSize)
        for (i <- 1 until m)
            {
            val shift = qc(i-1, infoSize)
            val v = if (shift >= 0)
                R(i) ^ (prev >> shift)
            else
                R(i)
            parityBits(i) = v
            prev = v
            }
        
        //done!
        val result = (dataBits ++ parityBits).map(_.bits).flatten
        result
        }
    
    def encodeBytes(byteArr: Array[Byte]) : Array[Byte] =
        {
        val bits = Bits.bytesToBits(byteArr)
        val encoded = encode(bits)
        encoded
        }
    
    def decode(bitArr: Array[Byte]) : Array[Byte] =
        {
        val size = bitArr.size
        
        val result = Array.ofDim[Byte](size)
        result
        }
}






/**
 * Some constants, definitions, and utilities for LDPC management and use.
 * 
 * 
 * @see http://en.wikipedia.org/wiki/Low-density_parity-check_code
 * The codes defined below are from the 802.11n standard, and are about
 * as close to being "standard" LDPC codes as one might get.  These are
 * "quasi-cyclic" codes, the term meaning they are generated from the iteration
 * of smaller blocks of 1's and 0'.
 * 
 * @see http://standards.ieee.org/getieee802/download/802.11n-2009.pdf
 */
object Ldpc
{

    /**
     * Creates an LDPC code matrix
     */
    def create(blockSize: Int, infoSize: Int, zSize: Int, vals: Int*) : Code =
        {
        val cols = 24
        val m = ByteMatrix(cols, vals:_*)
        new Code(m, blockSize, infoSize, zSize)
        }

    /*
     * @see http://standards.ieee.org/getieee802/download/802.11n-2009.pdf
     *
     * Table R.1: Matrix prototypes for codeword block length n=648 bits,
     * subblock size is Z = 27 bits
     */
    
    /**
     * (a) Coding rate R = 1/2.
     */
    val code0648_27_1_2 = create(648, 324, 27,
         0, -1, -1, -1,  0,  0, -1, -1,  0, -1, -1,  0,  1,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
        22,  0, -1, -1, 17, -1,  0,  0, 12, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
         6, -1,  0, -1, 10, -1, -1, -1, 24, -1,  0, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, -1, 
         2, -1, -1,  0, 20, -1, -1, -1, 25,  0, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, 
        23, -1, -1, -1,  3, -1, -1, -1,  0, -1,  9, 11, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, 
        24, -1, 23,  1, 17, -1,  3, -1, 10, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, 
        25, -1, -1, -1,  8, -1, -1, -1,  7, 18, -1, -1,  0, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, 
        13, 24, -1, -1,  0, -1,  8, -1,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, 
         7, 20, -1, 16, 22, 10, -1, -1, 23, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, 
        11, -1, -1, -1, 19, -1, -1, -1, 13, -1,  3, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, 
        25, -1,  8, -1, 23, 18, -1, 14,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, 
         3, -1, -1, -1, 16, -1, -1,  2, 25,  5, -1, -1,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0 
    )
    
    /**
     * (b) Coding rate R = 2/3.
     */
    val code0648_27_2_3 = create(648, 432, 27,
        25, 26, 14, -1, 20, -1,  2, -1,  4, -1, -1,  8, -1, 16, -1, 18,  1,  0, -1, -1, -1, -1, -1, -1, 
        10,  9, 15, 11, -1,  0, -1,  1, -1, -1, 18, -1,  8, -1, 10, -1, -1,  0,  0, -1, -1, -1, -1, -1, 
        16,  2, 20, 26, 21, -1,  6, -1,  1, 26, -1,  7, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, 
        10, 13,  5,  0, -1,  3, -1,  7, -1, -1, 26, -1, -1, 13, -1, 16, -1, -1, -1,  0,  0, -1, -1, -1, 
        23, 14, 24, -1, 12, -1, 19, -1, 17, -1, -1, -1, 20, -1, 21, -1,  0, -1, -1, -1,  0,  0, -1, -1, 
         6, 22,  9, 20, -1, 25, -1, 17, -1,  8, -1, 14, -1, 18, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, 
        14, 23, 21, 11, 20, -1, 24, -1, 18, -1, 19, -1, -1, -1, -1, 22, -1, -1, -1, -1, -1, -1,  0,  0, 
        17, 11, 11, 20, -1, 21, -1, 26, -1,  3, -1, -1, 18, -1, 26, -1,  1, -1, -1, -1, -1, -1, -1,  0
    )
    
    /**
     * (c) Coding rate R = 3/4.
     */
    val code0648_27_3_4 = create(648, 486, 27,
        16, 17, 22, 24,  9,  3, 14, -1,  4,  2,  7, -1, 26, -1,  2, -1, 21, -1,  1,  0, -1, -1, -1, -1, 
        25, 12, 12,  3,  3, 26,  6, 21, -1, 15, 22, -1, 15, -1,  4, -1, -1, 16, -1,  0,  0, -1, -1, -1, 
        25, 18, 26, 16, 22, 23,  9, -1,  0, -1,  4, -1,  4, -1,  8, 23, 11, -1, -1, -1,  0,  0, -1, -1, 
         9,  7,  0,  1, 17, -1, -1,  7,  3, -1,  3, 23, -1, 16, -1, -1, 21, -1,  0, -1, -1,  0,  0, -1, 
        24,  5, 26,  7,  1, -1, -1, 15, 24, 15, -1,  8, -1, 13, -1, 13, -1, 11, -1, -1, -1, -1,  0,  0, 
         2,  2, 19, 14, 24,  1, 15, 19, -1, 21, -1,  2, -1, 24, -1,  3, -1,  2,  1, -1, -1, -1, -1,  0 
    )
    
    /**
     * (d) Coding rate R = 5/6.
     */
    val code0648_27_5_6 = create(648, 540, 27,
        17, 13,  8, 21,  9,  3, 18, 12, 10,  0,  4, 15, 19,  2,  5, 10, 26, 19, 13, 13,  1,  0, -1, -1, 
         3, 12, 11, 14, 11, 25,  5, 18,  0,  9,  2, 26, 26, 10, 24,  7, 14, 20,  4,  2, -1,  0,  0, -1, 
        22, 16,  4,  3, 10, 21, 12,  5, 21, 14, 19,  5, -1,  8,  5, 18, 11,  5,  5, 15,  0, -1,  0,  0, 
         7,  7, 14, 14,  4, 16, 16, 24, 24, 10,  1,  7, 15,  6, 10, 26,  8, 18, 21, 14,  1, -1, -1,  0
    )
    
    /**
     * Table R.2: Matrix prototypes for codeword block length n=1296, bits,
     * subblock size is Z= 54, bits
     */
    
    /**
     * (a) Coding rate R = 1/2.
     */
    val code1296_54_1_2 = create(1296, 648, 54,
        40, -1, -1, -1, 22, -1, 49, 23, 43, -1, -1, -1,  1,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
        50,  1, -1, -1, 48, 35, -1, -1, 13, -1, 30, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
        39, 50, -1, -1,  4, -1,  2, -1, -1, -1, -1, 49, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, -1, 
        33, -1, -1, 38, 37, -1, -1,  4,  1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, 
        45, -1, -1, -1,  0, 22, -1, -1, 20, 42, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, 
        51, -1, -1, 48, 35, -1, -1, -1, 44, -1, 18, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, 
        47, 11, -1, -1, -1, 17, -1, -1, 51, -1, -1, -1,  0, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, 
         5, -1, 25, -1,  6, -1, 45, -1, 13, 40, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, 
        33, -1, -1, 34, 24, -1, -1, -1, 23, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, 
         1, -1, 27, -1,  1, -1, -1, -1, 38, -1, 44, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, 
        -1, 18, -1, -1, 23, -1, -1,  8,  0, 35, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, 
        49, -1, 17, -1, 30, -1, -1, -1, 34, -1, -1, 19,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0 
    )
    
    /**
     * (b) Coding rate R = 2/3.
     */
    val code1296_54_2_3 = create(1296, 864, 54,
        39, 31, 22, 43, -1, 40,  4, -1, 11, -1, -1, 50, -1, -1, -1,  6,  1,  0, -1, -1, -1, -1, -1, -1, 
        25, 52, 41,  2,  6, -1, 14, -1, 34, -1, -1, -1, 24, -1, 37, -1, -1,  0,  0, -1, -1, -1, -1, -1, 
        43, 31, 29,  0, 21, -1, 28, -1, -1,  2, -1, -1,  7, -1, 17, -1, -1, -1,  0,  0, -1, -1, -1, -1, 
        20, 33, 48, -1,  4, 13, -1, 26, -1, -1, 22, -1, -1, 46, 42, -1, -1, -1, -1,  0,  0, -1, -1, -1, 
        45,  7, 18, 51, 12, 25, -1, -1, -1, 50, -1, -1,  5, -1, -1, -1,  0, -1, -1, -1,  0,  0, -1, -1, 
        35, 40, 32, 16,  5, -1, -1, 18, -1, -1, 43, 51, -1, 32, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, 
         9, 24, 13, 22, 28, -1, -1, 37, -1, -1, 25, -1, -1, 52, -1, 13, -1, -1, -1, -1, -1, -1,  0,  0, 
        32, 22,  4, 21, 16, -1, -1, -1, 27, 28, -1, 38, -1, -1, -1,  8,  1, -1, -1, -1, -1, -1, -1,  0
    )
    
    /**
     * (c) Coding rate R = 3/4.
     */
    val code1296_54_3_4 = create(1296, 972, 54,
        39, 40, 51, 41,  3, 29,  8, 36, -1, 14, -1,  6, -1, 33, -1, 11, -1,  4,  1,  0, -1, -1, -1, -1, 
        48, 21, 47,  9, 48, 35, 51, -1, 38, -1, 28, -1, 34, -1, 50, -1, 50, -1, -1,  0,  0, -1, -1, -1, 
        30, 39, 28, 42, 50, 39,  5, 17, -1,  6, -1, 18, -1, 20, -1, 15, -1, 40, -1, -1,  0,  0, -1, -1, 
        29,  0,  1, 43, 36, 30, 47, -1, 49, -1, 47, -1,  3, -1, 35, -1, 34, -1,  0, -1, -1,  0,  0, -1, 
         1, 32, 11, 23, 10, 44, 12,  7, -1, 48, -1,  4, -1,  9, -1, 17, -1, 16, -1, -1, -1, -1,  0,  0, 
        13,  7, 15, 47, 23, 16, 47, -1, 43, -1, 29, -1, 52, -1,  2, -1, 53, -1,  1, -1, -1, -1, -1,  0
    )
    
    /**
     * (d) Coding rate R = 5/6.
     */
    val code1296_54_5_6 = create(1296, 1080, 54,
        48, 29, 37, 52,  2, 16,  6, 14, 53, 31, 34,  5, 18, 42, 53, 31, 45, -1, 46, 52,  1,  0, -1, -1, 
        17,  4, 30,  7, 43, 11, 24,  6, 14, 21,  6, 39, 17, 40, 47,  7, 15, 41, 19, -1, -1,  0,  0, -1, 
         7,  2, 51, 31, 46, 23, 16, 11, 53, 40, 10,  7, 46, 53, 33, 35, -1, 25, 35, 38,  0, -1,  0,  0, 
        19, 48, 41,  1, 10,  7, 36, 47,  5, 29, 52, 52, 31, 10, 26,  6,  3,  2, -1, 51,  1, -1, -1,  0
    )
    
    /**
     * Table R.3: Matrix prototypes for codeword block length n=1944, bits,
     * subblock size is Z = 81, bits
     */
    
    /**
     * (a) Coding rate R = 1/2.
     */
    val code1944_81_1_2 = create(1944, 972, 81,
        57, -1, -1, -1, 50, -1, 11, -1, 50, -1, 79, -1,  1,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
         3, -1, 28, -1,  0, -1, -1, -1, 55,  7, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
        30, -1, -1, -1, 24, 37, -1, -1, 56, 14, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, -1, 
        62, 53, -1, -1, 53, -1, -1,  3, 35, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, -1, 
        40, -1, -1, 20, 66, -1, -1, 22, 28, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, -1, 
         0, -1, -1, -1,  8, -1, 42, -1, 50, -1, -1,  8, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, 
        69, 79, 79, -1, -1, -1, 56, -1, 52, -1, -1, -1,  0, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, 
        65, -1, -1, -1, 38, 57, -1, -1, 72, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, 
        64, -1, -1, -1, 14, 52, -1, -1, 30, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, -1, 
        -1, 45, -1, 70,  0, -1, -1, -1, 77,  9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1, 
         2, 56, -1, 57, 35, -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  0, 
        24, -1, 61, -1, 60, -1, -1, 27, 51, -1, -1, 16,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0
    )
    
    /**
     * (b) Coding rate R = 2/3.
     */
    val code1944_81_2_3 = create(1944, 1296, 81,
        61, 75,  4, 63, 56, -1, -1, -1, -1, -1, -1,  8, -1,  2, 17, 25,  1,  0, -1, -1, -1, -1, -1, -1, 
        56, 74, 77, 20, -1, -1, -1, 64, 24,  4, 67, -1,  7, -1, -1, -1, -1,  0,  0, -1, -1, -1, -1, -1, 
        28, 21, 68, 10,  7, 14, 65, -1, -1, -1, 23, -1, -1, -1, 75, -1, -1, -1,  0,  0, -1, -1, -1, -1, 
        48, 38, 43, 78, 76, -1, -1, -1, -1,  5, 36, -1, 15, 72, -1, -1, -1, -1, -1,  0,  0, -1, -1, -1, 
        40,  2, 53, 25, -1, 52, 62, -1, 20, -1, -1, 44, -1, -1, -1, -1,  0, -1, -1, -1,  0,  0, -1, -1, 
        69, 23, 64, 10, 22, -1, 21, -1, -1, -1, -1, -1, 68, 23, 29, -1, -1, -1, -1, -1, -1,  0,  0, -1, 
        12,  0, 68, 20, 55, 61, -1, 40, -1, -1, -1, 52, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1,  0,  0, 
        58,  8, 34, 64, 78, -1, -1, 11, 78, 24, -1, -1, -1, -1, -1, 58,  1, -1, -1, -1, -1, -1, -1,  0
    )
    
    /**
     * (c) Coding rate R = 3/4.
     */
    val code1944_81_3_4 = create(1944,1458, 81,
        48, 29, 28, 39,  9, 61, -1, -1, -1, 63, 45, 80, -1, -1, -1, 37, 32, 22,  1,  0, -1, -1, -1, -1, 
         4, 49, 42, 48, 11, 30, -1, -1, -1, 49, 17, 41, 37, 15, -1, 54, -1, -1, -1,  0,  0, -1, -1, -1, 
        35, 76, 78, 51, 37, 35, 21, -1, 17, 64, -1, -1, -1, 59,  7, -1, -1, 32, -1, -1,  0,  0, -1, -1, 
         9, 65, 44,  9, 54, 56, 73, 34, 42, -1, -1, -1, 35, -1, -1, -1, 46, 39,  0, -1, -1,  0,  0, -1, 
         3, 62,  7, 80, 68, 26, -1, 80, 55, -1, 36, -1, 26, -1,  9, -1, 72, -1, -1, -1, -1, -1,  0,  0, 
        26, 75, 33, 21, 69, 59,  3, 38, -1, -1, -1, 35, -1, 62, 36, 26, -1, -1,  1, -1, -1, -1, -1,  0
    )
    
    /**
     * (d) Coding rate R = 5/6.
     */
    val code1944_81_5_6 = create(1944, 1620, 81,
        13, 48, 80, 66,  4, 74,  7, 30, 76, 52, 37, 60, -1, 49, 73, 31, 74, 73, 23, -1,  1,  0, -1, -1,
        69, 63, 74, 56, 64, 77, 57, 65,  6, 16, 51, -1, 64, -1, 68,  9, 48, 62, 54, 27, -1,  0,  0, -1,
        51, 15,  0, 80, 24, 25, 42, 54, 44, 71, 71,  9, 67, 35, -1, 58, -1, 29, -1, 53,  0, -1,  0,  0,
        16, 29, 36, 41, 44, 56, 59, 37, 50, 24, -1, 65,  4, 65, 52, -1,  4, -1, 73, 52,  1, -1, -1,  0
    )
    
}

