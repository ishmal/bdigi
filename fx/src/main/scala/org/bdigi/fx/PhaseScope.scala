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


package org.bdigi.fx

import javafx.application.Platform
import javafx.scene.paint.Color
import javafx.scene.canvas.{Canvas,GraphicsContext}
import org.bdigi.Complex


class PhaseScope extends Canvas(70,70)
{
    private val BUFSIZE = 512
    private val buf = Array.fill(BUFSIZE)(Complex(0.0))
    private var bufPtr = 0
    private var lastx = 0.0
    private var lasty = 0.0
    private val vscale = 6.0
    private val timeScale = 2
    
    private val g = getGraphicsContext2D

    var busy = false
    val refresher = new Runnable
        {
        override def run =
           {
           busy = true
           redraw
           busy = false
           }
        }

    def update(value: Complex) = 
        {
        buf(bufPtr) = value
        bufPtr = (bufPtr + 1) % BUFSIZE
        if (!busy)
            Platform.runLater(refresher)
        }
        
    private def redraw =
        {
        val w   = getWidth
        val w2  = w * 0.5
        val h   = getHeight
        val h2  = h * 0.5
        g.setFill(Color.BLACK)
        g.fillRect(0, 0, w, h)
        g.setStroke(Color.WHITE)
        g.strokeLine(0, h2, w, h2+2.0)
        g.strokeLine(w2, 0, w2, h)
        val x0  = w2
        val y0  = h2
        var ptr = bufPtr
        
        var x     = 0.0
        var y     = 0.0
        g.setStroke(Color.YELLOW)
        var v = Complex(0.0)
        for (i <- 0 until BUFSIZE by timeScale)
            {
            v = buf(ptr)
            ptr = (ptr + 1) % BUFSIZE
            x = x0 + v.r * vscale
            y = y0 + v.i * vscale
            g.strokeLine(lastx, lasty, x, y)
            lastx = x
            lasty = y
            }
        g.setFill(Color.RED)
        g.fillRect(x-2.0, y-2.0, 4.0, 4.0)
        }    
                

}
