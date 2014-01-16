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

package org.bdigi



trait Convolutional
{
    def k : Int
    
    def poly1: Int
    
    def poly2: Int
    
    /**
     * "output" contains 2 bits in positions 0 and 1 describing the state machine
     * for each bit delay, ie: for k = 7 there are 128 possible state pairs.
     * the modulo-2 addition for polynomial 1 is in bit 0
     * the modulo-2 addition for polynomial 2 is in bit 1
     * the allowable state outputs are 0, 1, 2 and 3
     */
    def output : Array[Int]

    protected def parity(v: Int) =
        Integer.bitCount(v) & 1

    protected def toBits(v: Int) : String =
        {
        var s = java.lang.Integer.toString(v, 2)
        if (s.size < 2) s = "0" + s
        s        
        }
    
    override def toString : String =
        {
        val buf = new StringBuilder
        buf.append("===== Viterbi decoder ======\n")
        buf.append("    k    : " + k + "\n")
        buf.append("    poly1: " + poly1 + "\n")
        buf.append("    poly2: " + poly2 + "\n")
        buf.append("    size : " + output.size + "\n")
        for (i <- 0 until output.size)
            {
            buf.append("    " + i + " : " + toBits(output(i)) + "\n")
            }
        buf.toString
        }

}



/**
 * Viterbi convolutional code decoder
 * @param k the constraint of the convolutional code
 * @param poly1 the left-hand generator polynomial
 * @param poly2 the right-hand generator polynomial   
 */
class ViterbiDecoder(val k: Int, val poly1: Int, val poly2: Int, chunkSize: Int = 8)
     extends Convolutional
{


    var lastMetric       = 0

    private val depth    = chunkSize * 8
    private val size     = 1 << k
    private val nrStates = 1 << (k - 1)
    private var currPtr  = 0
    private var prevPtr  = depth - 1
    private var metrics  = Array.ofDim[Int](depth, nrStates) //error metrics
    private var history  = Array.ofDim[Int](depth, nrStates) //the state history table
    private val seq      = Array.ofDim[Int](depth)

    val output = Array.tabulate(size)(i => 
        { (parity(poly1 & i) << 1) | (parity(poly2 & i)) })

    def printMetrics(distance: Int = 8) =
        {
        println("==== Metrics/History ====")
        var pp = prevPtr
        for (i <- 0 until distance)
            {
            for (j <- 0 until nrStates)
                {
                print("(%5d %5d) ".format(metrics(pp)(j), history(pp)(j)))                
                }
            println
            pp -= 1
            if (pp < 0)
                pp = depth - 1
            }            
        }
    
    def reset =
        {
        metrics = Array.ofDim[Int](depth, nrStates)
        history = Array.ofDim[Int](depth, nrStates)
        currPtr = 0
        prevPtr = depth - 1
        }
    
    /**
     * Used to provide a good Euclidean distance for symbols with values 0..255
     * The table is square, and is created for the origin 0,0 so
     * distance(00 -> xy) = table(x)(y).  
     * To get distances to the other three poles, just reverse the corresponding
     * values.     
     * So...
     * distance(01 -> xy) = output(x)(255-y)  
     * distance(10 -> xy) = output(255-x)(y)  
     * distance(11 -> xy) = output(255-x)(255-y)  
     *                              
     */
    val distanceTable =
        {
        val arr = Array.ofDim[Short](256, 256)
        for (i <- 0 until 256 ; j <- 0 until 256)
            {
            val dist : Short = math.round(math.sqrt((i * i) + (j * j)).toDouble).toShort
            arr(i)(j) = dist
            }
        arr
        }
    
    def traceback() : Seq[Boolean] =
        {
        /**
         * First, select the state having the smallest accumulated
         * error metric and save the state number of that state.
         * Since currPtr has been incremented after the last sample,
         * prevPtr points at the last recorded values.                  
         */         
        var min  = Int.MaxValue
        var best = 0
        for (i <- 0 until nrStates)
            {
            val v = metrics(prevPtr)(i)
            if (v < min) 
                {
                min  = v
                best = i
                }
            }
        //println("p: " + prevPtr + " best: " + best)
        
        /**
         * Working backward through the state history table, for the
         * selected state, select a new state which is listed in the
         * state history table as being the predecessor to that state.
         * Save the state number of each selected state. This step is
         * called traceback.      
         */
        var p = prevPtr
        var ps = best
        seq(p) = best
        for (i <- 1 until depth-1)
            {
            ps = history(p)(ps)
            //println("p: " + p + " ps:" + ps)
            seq(p) = ps
            p -= 1
            if (p < 0)
                p = depth-1
            }

        lastMetric = metrics(prevPtr)(best) - metrics(p)(ps)
        
        /**
         * Now work forward through the list of selected states saved in
         * the previous steps. Look up what input bit corresponds to a
         * transition from each predecessor state to its successor state.
         * That is the bit that must have been encoded by the convolutional
         * encoder.
         */                 
        val res = Array.fill(chunkSize)
            {
            val bval = ((seq(p) & 1) != 0)
            p += 1
            if (p >= depth)
                p = 0
            bval
            }
        
        res
        }


    /**
     * @param sym0 bit with range 0..255
     * @param sym1 bit with range 0..255
     * @param output function to call with metrics info
     * @return decoded symbol if successful, else -1          
     */         
    def decodeOne(sym0: Int, sym1: Int) : Seq[Boolean] =
        {

        /**
         * The decoding process begins with building the accumulated error
         * metric for some number of received channel symbol pairs, and the
         * history of what states preceded the states at each time instant t
         * with the smallest accumulated error metric. Once this information
         * is built up, the Viterbi decoder is ready to recreate the sequence
         * of bits that were input to the convolutional encoder when the
         * message was encoded for transmission.
         *          
         * Note that in our metric tables, larger number means smaller error.   
         * The output table is twice the size of nrStates, so check lower and upper
         * half and record the better value.                        
         */                 
        /**
         * For a soft decision decoder, a branch metric is measured using the
         * Euclidean distance. 
         *
         */
        val branchMetric = Array[Int](
            distanceTable(    sym0)(    sym1),
            distanceTable(    sym0)(255-sym1),
            distanceTable(255-sym0)(    sym1),
            distanceTable(255-sym0)(255-sym1)
            )       
            
        decodeOne(branchMetric)
        }
  
    /**
     * @param branchMetric.  An array with a positive integer distance
     * from each of 00, 01, 10, and 11
     * @param output function to call with metrics info
     * @return decoded symbol if successful, else -1          
     */         
    def decodeOne(branchMetric: Array[Int]) : Seq[Boolean] =
        {  
        //println("(%3d,%3d) : %4d %4d %4d %4d".format(sym0, sym1,
        //    branchMetric(0), branchMetric(1), branchMetric(2), branchMetric(3)))

        /**
         * For each state at this point in time, add the branch metric to
         * the metrics of two incoming states at the previous call.  Choose the lesser
         * of the two.  Record the metric, and indicate which incoming state was
         * chosen.
         */                  
        for (n <- 0 until nrStates)
            {
            val s0      = n
            val s1      = n + nrStates
            val p0      = s0 >> 1
            val p1      = s1 >> 1
            val metric0 = metrics(prevPtr)(p0) + branchMetric(output(s0))
            val metric1 = metrics(prevPtr)(p1) + branchMetric(output(s1))

            //println("p0: %d : %5d  p1: %d : %5d".format(p0, metric0, p1, metric1))
            if (metric0 < metric1)
                {
                metrics(currPtr)(n) = metric0
                history(currPtr)(n) = p0
                }
            else
                {
                metrics(currPtr)(n) = metric1
                history(currPtr)(n) = p1
                }
            
            }

        //advance to record these values
        prevPtr = currPtr
        currPtr = (currPtr + 1) % depth

        //have we received a complete chunk? Then process it and
        //return the decoded bits
        val res = if ((currPtr % chunkSize) == 0)
            {
            traceback()
            }
        else
            {
            //check if values are growing too large. if so, then adjust
            val halfMax = Int.MaxValue / 2
        
            if (metrics(currPtr)(0) > halfMax)
                {
                for (i <- 0 until depth; j <- 0 until nrStates)
                    metrics(i)(j) -= halfMax
                }
            if (metrics(currPtr)(0) < -halfMax)
                {
                for (i <- 0 until depth; j <- 0 until nrStates)
                    metrics(i)(j) += halfMax
                }
            List[Boolean]()
            }
    
        res
        }
    


    /**
     * @param syms, seq of sym0, sym1 typles
     * @param finish whether we want to flush the buffer
     * @return decoded bits
     */         
    def decode(syms: Seq[(Int,Int)]) : Seq[Boolean] =
        {
        syms.map(sym=> decodeOne(sym._1, sym._2)).flatten
        }
    
    /**
     * @param dibit two bits in the 0 and 1 position
     * @param finish whether we want to flush the buffer
     * @return decoded symbol if mod of chunkSize or we want to flush          
     */         
    def decodeOneHard(dibit: Int) : Seq[Boolean] =
        {
        //255 for true 0 for false.  hard decisions
        val sym1 = if ((dibit & 1) != 0) 255 else 0
        val sym0 = if ((dibit & 2) != 0) 255 else 0

        decodeOne(sym0, sym1)
        }
    
    def decodeHard(dibits: Seq[Int]) : Seq[Boolean] =
        {
        dibits.map(decodeOneHard).flatten
        }
    
}





class ConvolutionalEncoder(val k: Int, val poly1: Int, val poly2: Int)
     extends Convolutional
{

    private val size = 1 << k

    val output = Array.tabulate(size)(i => 
        { (parity(poly1 & i) << 1) | (parity(poly2 & i)) })
    private var state = 0
    private val statemask = size - 1

    def reset =
        {
        state = 0
        }

    def encode(bit: Boolean) : Int =
        {
        val bval = if (bit) 1 else 0 
        state = ((state << 1) | bval) & statemask
        output(state)
        }

    def encodeBits(bits: Seq[Boolean]) : Seq[Int] =
        {
        bits.map(encode)
        }

    def encodeWord(word: Int, cnt: Int = 8) : Seq[Int] =
        {
        var c = cnt - 1
        val arr = Array.fill(cnt)(
            {
            val v = (((word >> c) & 1) != 0)
            c -= 1
            v
            })
        encodeBits(arr)
        }

    def encodeWords(words: Seq[Int], cnt: Int = 8) : Seq[Int] =
        {
        words.map(b=> encodeWord(b, cnt)).flatten
        }

    def encodeBytes(bytes: Seq[Byte]) : Seq[Int] =
        {
        bytes.map(b=> encodeWord(b, 8)).flatten
        }

    def encodeStr(str: String) : Seq[Int] =
        {
        encodeBytes(str.getBytes)
        }
}




object Viterbi
{
    def encoder(k: Int, poly1: Int, poly2: Int) =
        new ConvolutionalEncoder(k, poly1, poly2)

    def decoder(k: Int, poly1: Int, poly2: Int, chunksize: Int = 8) =
        new ViterbiDecoder(k, poly1, poly2, chunksize)

    def toBits(v: Int) : String =
        {
        var s = java.lang.Integer.toString(v, 2)
        if (s.size < 2) s = "0" + s
        s        
        }
    
    def fromBits(bits: Seq[Boolean], size: Int = 8) :  Seq[Int] =
        {
        var buf = List[Int]()
        var cnt = 0
        var c = 0
        for (b <- bits)
            {
            c <<= 1
            if (b) c += 1
            cnt += 1
            if (cnt >= size)
                {
                cnt = 0
                buf ::= c
                c = 0
                }
            }
        buf.reverse
        }


}
