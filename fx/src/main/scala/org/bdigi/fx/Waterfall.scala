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


//import java.awt.{Color,Dimension,Image,Point,RenderingHints}
//import java.awt.geom.{Rectangle2D}

import org.bdigi.{App, Complex, Constants, DFft, Log, MathUtil, Window}

import javafx.application.Platform
import javafx.beans.value.{ChangeListener,ObservableValue}
import javafx.scene.layout.{AnchorPane,HBox,VBox,Pane}
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.{ImageView,WritableImage,PixelFormat}
import javafx.scene.shape.{Rectangle}
import javafx.scene.paint.Color
import javafx.event.{Event, EventHandler}
import javafx.scene.input.{KeyEvent,MouseEvent,ScrollEvent}

//########################################################################
//#    W A T E R F A L L
//########################################################################



class AudioWaterfall(par: App) extends Pane
{

    class Waterfall(width: Double, height: Double) extends Canvas(width, height)
        {
        val N = 4096
        val frame = Array.fill(N)(0.0)
        val bins = (1.0 * maxFreq / par.sampleRate * N).toInt
        Log.trace("wf samplerate: " + par.sampleRate + "  bins:" + bins)
        val window = Window.Hamming(N)
        val iwidth = width.toInt
        val iheight = height.toInt
    
        val img = new WritableImage(iwidth, iheight)
        val nrPix = iwidth * iheight
        val pixels = Array.ofDim[Int](nrPix)
        val lastRow = nrPix - iwidth
        val writer = img.getPixelWriter
        val format = PixelFormat.getIntArgbInstance
        val g2d = getGraphicsContext2D
                
        private val colors = Array.tabulate(256)( i =>
            {
            val prop   = i.toDouble / 256.0
            val hue    = 240.0 - 150.0 * prop
            val bright = 1.0 // 0.3 + prop / 2
            val c = Color.hsb(hue, 1.0, bright)
            var col = 0xff
            col = (col << 8) + (c.getRed   * 255).toInt
            col = (col << 8) + (c.getGreen * 255).toInt
            col = (col << 8) + (c.getBlue  * 255).toInt
            col
            })
            
        /**
         * Make a palette. tweak this often
         */                 
        private val colors2 = Array.tabulate(256)( i=>
            {
            val r = if (i < 170) 0 else (i-170) * 3
            val g = if (i <  85) 0 else if (i < 170) (i-85) * 3 else 255
            val b = if (i <  85) i * 3 else 255
            var col = 0xff
            col = (col << 8) + r
            col = (col << 8) + g
            col = (col << 8) + b
            col
            })
            
        var busy = false
        val refresher = new Runnable
            {
            override def run = 
                {
                busy = true
                g2d.drawImage(img, 0.0, 0.0, width, height)
                busy = false
                }
            }
    
        /*Scale the power spectrum bins onto the output width.  Do once & reuse. */
        private val psIndices = Array.tabulate(iwidth)(_ * bins / iwidth)
                
            
        def redraw(ps: Array[Double]) =
            {
            val pix = pixels
            System.arraycopy(pix, iwidth, pix, 0, lastRow)
            var pixptr = lastRow
            for (i <- 0 until iwidth)
                {
                val p = ps(psIndices(i))
                val v = MathUtil.log1p(p) * 40.0
                val colidx = v.toInt & 0xff
                val col = colors(colidx)
                pix(pixptr) = colors(colidx)
                pixptr += 1
                }
            //trace("iw:" + iwidth + "  ih:" + iheight + "  pix:" + pix.size + " pslen:" + pslen)
            writer.setPixels(0, 0, iwidth, iheight, format, pix, 0, iwidth)
            if (!busy)
                Platform.runLater(refresher)
            }
        
        
        val trans = new DFft(N)
        
        private var framePtr = 0
        private var frameCtr = 0
        private val SUBN = N/5
        
        private val slidingbuf = Array.ofDim[Double](N)
        
        def update2(v: Double) =
            {
            frame(framePtr) = v * window(framePtr)
            framePtr += 1
            if (framePtr >= N)
                {
                framePtr = 0
                val ps = trans.powerSpectrum(frame, bins)
                redraw(ps)
                }
            }
            
        def update(v: Double) =
            {
            frame(framePtr) = v
            framePtr = (framePtr + 1) % N
            frameCtr += 1
            if (frameCtr >= SUBN)
                {
                frameCtr = 0
                var fp = (framePtr + 1) % N
                for (i <- 0 until N)
                    {
                    slidingbuf(i) = frame(fp)  * window(i)
                    fp = (fp + 1) % N
                    }
                val ps = trans.powerSpectrum(slidingbuf, bins)
                redraw(ps)
                }
            }
            
    
        }//Waterfall
    
    class Tuner(width: Double, height: Double) extends Canvas(width, height)
        {
        val g = getGraphicsContext2D
        val range = maxFreq - minFreq
            
        def freq = par.frequency
        def freq_=(v: Double) =
            {
            par.frequency = v
            }
            
        def bw = par.mode.bandwidth
            
        def drawMe = 
            {
            val top     = 0
            
            g.setFill(Color.BLACK)
            g.fillRect(0, 0, width.toInt, height.toInt)
            
            //draw the tickmarks
            val hzWidth   = width / range
            val tickRes   = 25
            val tickSpace = hzWidth * tickRes
            val nrTicks = (range / tickRes).toInt
    
            for (i <- 1 until nrTicks)
                {
                val tick = i * tickRes
                val hx = i * tickSpace
                if (tick % 500 == 0)
                    {
                    g.setFill(Color.GREEN)
                    g.fillRect(hx, top, 2.0, 10.0)
                    val str = "%d".format(tick)
                    g.setFill(Color.CYAN)
                    g.fillText(str, hx-16.0, 19.0)
                    }
                else if (tick % 100 == 0)
                    {
                    g.setFill(Color.GREEN)
                    g.fillRect(hx, top, 2.0, 5.0)
                    }
                else
                    {
                    g.setFill(Color.GREEN)
                    g.fillRect(hx, top, 2.0, 2.0)
                    }
                }
    
            
            g.setFill(Color.GREEN)
            val fx = width * freq / range
            g.fillRect(fx, 3, 2.0, 10)
            
            if (par.bandwidth > 0.0)
                {
                g.setFill(Color.RED)
                val lox = width * (freq - bw * 0.5) / range
                g.fillRect(lox, 5, 1.0, 10)
                val hix = width * (freq + bw * 0.5) / range
                g.fillRect(hix, 5, 1.0, 10)
                }
            }
            
        drawMe
        
        def x2freq(x: Double) =
            {
            range * x / width
            }
        
        
        addEventHandler(MouseEvent.ANY, new EventHandler[MouseEvent]
            {
            def handle(evt: MouseEvent)
                {
                evt.getEventType match
                    {
                    case MouseEvent.MOUSE_CLICKED => freq = x2freq(evt.getX) ; drawMe
                    case MouseEvent.MOUSE_DRAGGED => freq = x2freq(evt.getX) ; drawMe
                    case _ =>
                    }
                }
            }) 
            
        setOnScroll(new EventHandler[ScrollEvent]
            {
            def handle(evt: ScrollEvent)
                {
                freq =  if (evt.getDeltaY > 0) freq + 1 else freq - 1 
                drawMe
                }
            });
        
        }//Tuner
    

    class Scope(width: Double, height: Double) extends Canvas(width, height)
        {
        private val BUFSIZE = 512
        private val buf = Array.fill(BUFSIZE)((0.0,0.0))
        private var bufPtr = 0
        private var lastx = 0.0
        private var lasty = 0.0
        private val vscale = 40.0
        private val timeScale = 2
        
        private val g = getGraphicsContext2D
    
        private var busy = false
        val refresher = new Runnable
            {
            override def run =
                {
                busy = true
                //trace("run")
                redraw
                busy = false
                }
            }
    
        def update(x: Double, y:Double) = 
            {
            buf(bufPtr) = (x, y)
            bufPtr = (bufPtr + 1) % BUFSIZE
            if ((bufPtr & 0xf) == 0) //every 16
                {
                if (!busy)
                    Platform.runLater(refresher)
                }
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
            for (i <- 0 until BUFSIZE by timeScale)
                {
                val v = buf(ptr)
                val vx = v._1
                val vy = v._2
                ptr = (ptr + 1) % BUFSIZE
                x = x0 + vx * vscale
                y = y0 + vy * vscale
                g.strokeLine(lastx, lasty, x, y)
                lastx = x
                lasty = y
                }
            g.setFill(Color.RED)
            g.fillRect(x-2.0, y-2.0, 4.0, 4.0)
            }    
                  
    } //scope
    

    AnchorPane.setLeftAnchor(this, 0)
    AnchorPane.setTopAnchor(this, 0)
    AnchorPane.setRightAnchor(this, 0)
    AnchorPane.setBottomAnchor(this, 0)

    val initialW = 620
    val initialH = 90
    val tunerH   = 20
    
    val minFreq = 0.0
    val maxFreq = 2500.0
    
    var wf = new Waterfall(initialW-initialH, initialH-tunerH)
    wf.relocate(0,0)
    var tuner = new Tuner(initialW-initialH, tunerH)
    tuner.relocate(0, initialH - tunerH)
    var scope = new Scope(initialH, initialH)
    scope.relocate(initialW-initialH, 0)
    getChildren.addAll(wf, tuner, scope)
    
    override def layoutChildren =
        {
        val width = getWidth
        val height = getHeight
        getChildren.clear
        wf = new Waterfall(width-height, height-tunerH)
        wf.relocate(0,0)
        tuner = new Tuner(width-height, tunerH)
        tuner.relocate(0, height-tunerH)
        scope = new Scope(height, height)
        scope.relocate(width-height, 0)
        getChildren.addAll(wf, tuner, scope)
        }

    def update(v: Double) =
        wf.update(v)

    def updateScope(x: Double, y: Double) =
        scope.update(x, y)

}




