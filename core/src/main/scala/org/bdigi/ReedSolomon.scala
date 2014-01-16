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



/*
class GField(primitive: Int)
{
    private var x = 1
    val expTable = Array.fill(256)
        {
        val v = x
        x <<= 1
        if (x >= 0x100)
            x^= primitive
        v
        }
        
    val logTable = Array.ofDim[Int](256)
    for (i <- 0 until 255)
        logTable(expTable(i)) = i

    val zero = new GPoly(this, Array(0))
    val one  = new GPoly(this, Array(1))
    
    def monomial(degree:Int, coefficient: Int) =
        {
        if (coefficient == 0)
            zero
        else
            {
            val arr = Array.ofDim[Int](degree + 1)
            arr(0) = coefficient
            new GPoly(this, arr)
            }
        }
        
    def exp(a: Int) = expTable(a)
    
    def log(a: Int) = logTable(a)
    
    def inv(a: Int) = expTable(255 - logTable(a))
    
    def mul(a: Int, b: Int) =
        {
        if (a == 0 || b == 0)
            0
        else if (a == 1)
            b
        else if (b == 1)
            a
        else
            expTable((logTable(a) + logTable(b)) % 255)
        }
    
}

object GField
{
    lazy val QR_CODE_FIELD = new GField(0x011D)
    lazy val DATA_MATRIX_FIELD = new GField(0x012D)
}

class GPoly(val field: GField, val coefficients: Array[Int])
{
    val coeffs : Array[Int] = 
        {
        if (coefficients.size > 1 && coefficients(0)==0)
            {
            val firstNonZero = coefficients.indexWhere(_ != 0)
            if (firstNonZero < 0) 
                field.zero.coeffs
            else
                coefficients.takeRight(coefficients.size - firstNonZero)
            }
        else
            coefficients
        }
        
    val size = coeffs.size
    val degree = size - 1
    val isZero = coeffs(0) == 0
    def coeff(degree: Int) = coeffs(size - 1 - degree)
    
    def eval(a: Int) =
        {
        if (a == 0)
            coeff(0)
        else if (a == 1)
            {
            coeffs.foldLeft(0)(_ ^ _)
            }
        else
            {
            coeffs.foldLeft(0)((acc,v) => field.mul(a, acc) ^ v)
            }
        }
        
    private def parcheck(other: GPoly) =
        if (field != other.field)
            throw new IllegalArgumentException("two GPolys do not have the same field")
        
    def addOrSubtract(other: GPoly) : GPoly =
        {
        parcheck(other)
        if (isZero)
            other
        else if (other.isZero)
            this
        else
            new GPoly(field, coeffs.zipAll(other.coeffs, 0, 0).map(p=>p._1 ^ p._2))
        }
        
    def +(other: GPoly) = addOrSubtract(other)
    def -(other: GPoly) = addOrSubtract(other)
    
    
    def *(other: GPoly) : GPoly =
        {
        parcheck(other)
        if (isZero || other.isZero)
            field.zero
        else
            {
            val product = Array.ofDim[Int](size + other.size - 1)
            for (i <- 0 until size)
                {
                val coeff = coeffs(i)
                for (j <- 0 until other.size)
                    product(i+j) ^= field.mul(coeff, other.coeffs(j))
                }
            new GPoly(field, product)
            }
        }
        
    def *(scalar: Int) : GPoly =
        {
        if (scalar == 0)
            field.zero
        else if (scalar == 1)
            this
        else
           new GPoly(field, coeffs.map(p=> field.mul(p, scalar)))
        }

    def *(degree: Int, coeff: Int) : GPoly =
        {
        if (coeff == 0)
            field.zero
        else
            new GPoly(field, coeffs.map(p=> field.mul(p, coeff)).padTo(size + degree, 0))
        }

    def divide(other: GPoly) : (GPoly,GPoly) =
        {
        parcheck(other)
        if (other.isZero)
            throw new IllegalArgumentException("Divide by zero")
        var quotient = field.zero
        var remainder = this
        val denominatorLeadingTerm = other.coeff(other.degree)
        val inverseDenominatorLeadingTerm = field.inv(denominatorLeadingTerm)

        while (remainder.degree >= other.degree && !remainder.isZero) 
            {
            val degreeDifference = remainder.degree - other.degree
            val scale = field.mul(remainder.coeff(remainder.degree), inverseDenominatorLeadingTerm)
            val term = other *(degreeDifference, scale)
            val iterationQuotient = field.monomial(degreeDifference, scale)
            quotient = quotient - iterationQuotient
            remainder = remainder - term
            }
        (quotient, remainder)
        }

}


class ReedSolomonEncoder(val primitive: Int)
{
    val field = new GField(primitive)
    
    private val generators = scala.collection.mutable.ListBuffer[GPoly]()
    
    def generator(degree: Int) : GPoly =
        {
        if (degree >= generators.size)
            {
            var last = generators.last
            for (d <- generators.size to degree)
                {
                val next = last * (new GPoly(field, Array(1, field.exp(d-1))))
                generators += next
                last = next
                }
            }
        generators(degree)
        }
        
    def encode(data: Array[Int], nrCorr: Int) : Array[Int] =
        {
        val size = data.size
        if (nrCorr == 0 || nrCorr >= size)
            throw new IllegalArgumentException("Incorrect # correction bytes specified")
        val gen       = generator(nrCorr)
        val info      = new GPoly(field, data)
        val info2     = info * (nrCorr, 1)
        val remainder = info2.divide(gen)._2
        val ecbytes   = remainder.coeffs
        val pad       = nrCorr - ecbytes.size
        data ++ Array.fill(pad)(0) ++ ecbytes
        }
        
}


class ReedSolomonDecoder(val primitive: Int)
{
    val field = new GField(primitive)
    
    def decode(in: Array[Int], nrCorr: Int) : Option[Array[Int]] =
        {
        val data = in.take(in.size - nrCorr)
        val poly = new GPoly(field, in)
        val synCoeffs = Array.tabulate(nrCorr)( i=> poly.eval(field.exp(nrCorr-i)))
        if (synCoeffs.exists(_ !=0))
            {
            val syn = new GPoly(field, synCoeffs)
            val sigmaOmega = euclidean(field.monomial(nrCoeffs, 1) syn, nrCoeff)
            val sigma = sigmaOmega._1
            val omega = sigmaOmega._2
            val errLocs = findErrorLocations(sigma)
            val errMags = findErrorMagnitudes(omega, errLocs)
            for (i <- 0 until errLocs.size)
                {
                val pos = in.size - 1 -field.log(errLocs(i))
                if (pos < 0)
                   throw new Exception("Bad error location")
                data(pos) ^= errMags(i)
                }
            }
        data
        }
        
    


}

*/




/**
 * The 3 specifications for a Reed-Solomon code
 *  
 * Note, jt65 uses RS(63, 55) with a poly of 0x43: x^6 + x + 1
 * Instantiate this with params: (63, 55, 1, 1) 
 * 
 * CCDSS is (255,223) is 0x87: x^8 + x^7 + x^2 + x^1 + 1
 * Instantiate this with params: (225, 223, 0x87, 112, 11) 
 */
class ReedSolomon(size: Int, dataSize: Int, gfPoly: Int, fcr: Int = 1, prim: Int = 1) 
{
    val power =   // order of the code
        {
        var pwr = 0
        var siz = size
        while (siz > 0)
            { siz >>= 1 ; pwr += 1}
        pwr
        }
    println("=== power:" + power)
    val bits       = power

    val checkSize  = size - dataSize    //number of check bits
    val tt         = checkSize / 2      //errors that can be corrected
    
    //These tables describe the galois field according to
    //the primitivePoly
    val alphaTo    = Array.ofDim[Int](size + 1)
    val indexOf    = Array.ofDim[Int](size + 1)   

    private var mask = 1
    private val msb  = 1 << power
    for (i <- 0 until size)
        {
        alphaTo(i)    = mask
        indexOf(mask) = i
        mask <<= 1
        if ((mask & msb) != 0) //wrap around
            mask ^= gfPoly;  
        mask &= size 
        }
    indexOf(0) = -1  //Error.  See encodeBlock() for how this is used
    
    if (mask != 1)
        throw new IllegalArgumentException("poly is not a primitive: " + gfPoly)

    private def mod(x: Int) : Int =
        {
        var v = x
        while (v >= size)
            {
            v -= size
            v = (v >> power) + (v & size)
            }
        v
        }
        
    
    /**
     * The inverse of the prim, used for decoding
     */         
    val iprim =
        {
        var ip = 1
        while ((ip % prim) != 0)
            ip += size
        ip / prim
        }
                
   /**
     *  Obtain the generator polynomial of the tt-error correcting, length
     *  nn=(2**mm -1) Reed Solomon code  from the product of (X+alpha**i), i=1..2*tt
     */
    val genPoly =
        {
        val gp = Array.ofDim[Int](checkSize+1)
        gp(0) = 1
        var root = fcr * prim
        for (i <- 0 until checkSize)
            { 
            gp(i + 1) = 1
            for (j <- i until 0 by -1)
                {
                if (gp(j) != 0)
                    gp(j) = gp(j-1) ^ alphaTo(mod(indexOf(gp(j)) + root))
                else
                    gp(j) = gp(j-1)
                }
            // genPoly[0] can never be zero
            gp(0) = alphaTo(mod(indexOf(gp(0)) + root))
            root += prim
            }

        println("genPoly1: " + gp.mkString(", "))
        // convert genPoly to index form for quicker encoding
        gp.map(indexOf)
        }
            
 
    override def toString : String =
        {
        val buf = new StringBuilder
        buf.append("=================================\n")
        buf.append("size: %d datasize: %d power: %d\n".format(size, dataSize, power))
        buf.append("=================================\n")
        for (i <- 0 until size+1)
            buf.append("%2d : %02x : %02x\n".format(i, alphaTo(i), indexOf(i)))
        buf.append("=================================\n")
        buf.append("genPoly: " + genPoly.map(alphaTo).mkString(", ") + "\n")
        buf.append("=================================\n")
        buf.toString
        }


    /** 
     * take the string of symbols in data[i], i=0..(k-1) and encode systematically
     * to produce 2*tt parity symbols in bb[0]..bb[2*tt-1]
     * data[] is input and bb[] is output in polynomial form.
     * Encoding is done by using a feedback shift register with appropriate
     * connections specified by the elements of gg[], which was generated above.
     * Codeword is   c(X) = data(X)*X**(nn-kk)+ b(X)
     */
    def encodeBlock(data: Array[Int]) : Array[Int] =
        {
        val csize = checkSize / 4
        var feedback = 0
        val check = Array.ofDim[Int](csize) 
        
        //
        for (i <- 0 until data.size)
            {  
            feedback = indexOf(data(i) ^ check(0))
            if (feedback != -1)
                {
                //feedback = mod(size - genPoly(checkSize) + feedback)
                for (j <- 1 until csize)
                    check(j) ^= alphaTo(mod(feedback + genPoly(csize - j)))
                }
            //shift left
            for (i <- 1 until csize)
                check(i-1) = check(i)
            //fill rightmost slot
            if (feedback != -1)
                check(csize-1) = alphaTo(mod(feedback + genPoly(0)))
            else
                check(csize-1) = 0
            }
        check
        }

    /** 
     */
    def encode(indata: Array[Int], append: Boolean = true) : Array[Int] =
        {
        println("data size " + dataSize)
        val pad = dataSize - indata.size - 1
        val data = 
            {
            if (append)
                indata ++ Array.fill(pad)(0)
            else
                Array.fill(pad)(0) ++ indata
            }
        encodeBlock(data) ++ indata
        }



    /**
     * Assume we have received bits grouped into mm-bit symbols in recd[i],
     * i=0..(nn-1).
     * We first compute the 2*tt syndromes by substituting alpha**i into rec(X) and
     * evaluating, storing the syndromes in s[i], i=1..2tt (leave s[0] zero) .
     *      
     * Then we use the Berlekamp iteration to find the error location polynomial
     * elp[i].   If the degree of the elp is >tt, we cannot correct all the errors
     * and hence just put out the information symbols uncorrected. If the degree of
     * elp is <=tt, we substitute alpha**i , i=1..n into the elp to get the roots,
     * hence the inverse roots, the error location numbers. If the number of errors
     * located does not equal the degree of the elp, we have more than tt errors
     * and cannot correct them.  Otherwise, we then solve for the error value at
     * the error location and correct the error.
     * 
     * The procedure is that found in Lin and Costello. For the cases where the
     * number of errors is known to be too large to correct, the information 
     * symbols as received are output (the advantage of systematic encoding is
     * that hopefully some of the information symbols will be okay and that if
     * we are in luck, the errors are in the parity part of the transmitted 
     * codeword).  Of course, these insoluble cases can be returned as error
     * flags to the calling routine if desired.
     */
    def decodeBlock(data: Array[Int], erasures: Array[Int]) : Option[Array[Int]] =
        {
        if (data.size != size)
            throw new IllegalArgumentException("Wrong data size: " + data.size +
            ".  Expected: " + size)
            
        val pad = 0 //fixme.  do we expect padded data, or do we account for a pad here?

        // first form the syndromes
        val d0 = data(0)
        var s = Array.fill(checkSize)(d0)

        for (j <- 1 until size)
            {
            for (i <- 0 until checkSize)
                {
                if (s(i) == 0)
                    s(i) = data(j)
                else
                    s(i) = data(j) ^ alphaTo(mod(indexOf(s(i)) + (fcr+i) * prim))
                }
            }

        if (s.forall(_ == 0))
            {
            // no non-zero syndromes => no errors, nothing to correct
            // output received codeword
            println("no errors")
            return Some(data)
            }

        s = s.map(indexOf)

        var lambda = Array.ofDim[Int](checkSize+1)
        lambda(0) = 1

        if (erasures.size > 0)
            {
            // Init lambda to be the erasure locator polynomial
            lambda(1) = alphaTo(mod(prim * (size-1-erasures(0))))
            for (i <- 0 until erasures.size)
                {
                val u = mod(prim * (size - 1 - erasures(i)))
                for (j <- i+1 until 0 by -1)
                    {
                    val tmp = indexOf(lambda(j-1))
                    if (tmp != -1)
                        lambda(j) ^= alphaTo(mod(u + tmp))
                    }
                }
            }

        //initialize the output
        val b = lambda.map(indexOf)
  
        /*
         * Begin Berlekamp-Massey algorithm to determine error+erasure
         * locator polynomial
         */
        var discr_r = 0
        var el = erasures.size
        var r = 0
        for (r <- erasures.size+1 to checkSize)
            {
            // Compute discrepancy at the r-th step in poly-form
            discr_r = 0
            for (i <- 0 until r)
                {
                if ((lambda(i) != 0) && (s(r-i-1) != -1))
                    discr_r ^= alphaTo(mod(indexOf(lambda(i)) + s(r-i-1)))
                }
           }

        discr_r = indexOf(discr_r)  // Index form
        if (discr_r == -1) 
            {
            // 2 lines below: B(x) <-- x*B(x)
            for (i <- 1 until b.size)
                b(i) = b(i-1)
            b(0) = -1
            }
        else 
            {
            // 7 lines below: T(x) <-- lambda(x) - discr_r*x*b(x)
            val t = Array.ofDim[Int](lambda.size)
            t(0) = lambda(0)
            for (i <- 0 until checkSize) 
                {
	            if (b(i) != -1)
	                t(i+1) = lambda(i+1) ^ alphaTo(mod(discr_r + b(i)))
	            else
	                t(i+1) = lambda(i+1)
                }
            if (2 * el <= r + erasures.size - 1)
                {
	            el = r + erasures.size - el
	            // 2 lines below: B(x) <-- inv(discr_r) * lambda(x)
	            for (i <- 0 to checkSize)
	                b(i) = if (lambda(i) == 0) -1 else mod(indexOf(lambda(i)) - discr_r + size)
                }
            else
                {
                // 2 lines below: B(x) <-- x*B(x) 
                for (i <- 1 until b.size)
                    b(i) = b(i-1)
                b(0) = -1
                }
            lambda = t
            }


        // Convert lambda to index form and find deg(lambda(x)) 
        lambda = lambda.map(indexOf)
        val deg_lambda = lambda.indexWhere(_ != (-1))
    
        // Find roots of the error+erasure locator polynomial by Chien search 
        val root = scala.collection.mutable.ListBuffer[Int]()
        val loc  = scala.collection.mutable.ListBuffer[Int]()
        val reg = lambda.clone
        reg(0)  = 0
        var count   = 0        // Number of roots of lambda(x) 
        var k   = iprim -1
        for (i <- 1 to size)
            {
            var q = 1 // lambda(0) is always 0 
            for (j <- deg_lambda until 0 by -1)
                {
                if (reg(j) != -1)
                    {
                    reg(j) = mod(reg(j) + j)
                    q ^= alphaTo(reg(j))
                    }
                }
            if (q == 0) //is this a root?
                {
                // store root (index-form) and error location number 
                println("count: " + count + " root: " + i + " loc: " + k)
                root.append(i)
                loc.append(k)
                count += 1    
                // If we've already found max possible roots, abort the search to save time
                //if(count == deg_lambda) break
                }
            k = mod(k+iprim)
            }
    
        //deg(lambda) unequal to number of roots => uncorrectable error detected
        if (deg_lambda != count)
            {
            count = -1
            return None
            }

        // @see http://en.wikipedia.org/wiki/Forney_algorithm
        //  omega is the error evaluator polynomial, calculated by:
        //  omega(x) = s(x)*lambda(x) (modulo x**checkSize). 
        //  in index form. Also find deg(omega).   
        var deg_omega = deg_lambda-1
        val omega = Array.ofDim[Int](deg_omega+1)
        for (i <- 0 to deg_omega)
            {
            var sum = 0
            for(j <- i to 0 by -1)
                {
                if ((s(i - j) != -1) && (lambda(j) != -1))
                    sum ^= alphaTo(mod(s(i - j) + lambda(j)))
                }
            omega(i) = indexOf(sum)
            }

        // Compute error values in poly-form. num1 = omega(inv(X(l))), num2 =
        // inv(X(l))**(FCR-1) and den = lambda_pr(inv(X(l))) all in poly-form
        for (j <- count-1 to 0 by -1)
            {
            var num1 = 0
            for (i <- deg_omega to 0 by -1)
                {
                if (omega(i) != -1)
                    num1  ^= alphaTo(mod(omega(i) + i * root(j)))
                }
            var num2 = alphaTo(mod(root(j) * (fcr - 1) + size))
            var den = 0
        
            // lambda(i+1) for i even is the formal derivative lambda_pr of lambda(i) 
            val start = deg_lambda min (checkSize-1)
            for (i <- start to 0 by -2)
                {
                if (lambda(i+1) != -1)
                    den ^= alphaTo(mod(lambda(i+1) + i * root(j)))
                }
    
            // Apply error to data 
            if (num1 != 0 && loc(j) >= pad)
                {
                data(loc(j) - pad) ^= alphaTo(mod(indexOf(num1) +
                     indexOf(num2) + size - indexOf(den)))
                }
            
            }
        
        println("fixed errors:" + count)
        
        Some(data)
        }// decodeBlock


    /** 
     * Remember that if we pad in encode(), then we must do it
     * the same way here.         
     */
    def decode(indata: Array[Int], append: Boolean = true) : Option[Array[Int]] =
        {
        val pad = size - indata.size
        val dsize = dataSize - pad
        val data = if (append)
                indata.take(dsize) ++ Array.fill(pad)(0) ++ indata.takeRight(checkSize)
            else
                Array.fill(pad)(0) ++ indata
        val res = decodeBlock(data, Array.ofDim[Int](0))
        if (res.isDefined)
            {
            val out = if (append) res.get.take(dsize) else res.get.slice(pad, dsize)
            Some(out)
            }
        else
            None
        }

}















