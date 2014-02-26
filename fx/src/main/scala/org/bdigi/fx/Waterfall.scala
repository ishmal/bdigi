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

import org.bdigi.{App, Complex, Constants, DFft, MathUtil, Window}

import javafx.application.Platform
import javafx.animation.{Animation, KeyFrame, TimelineBuilder}
import javafx.util.Duration
import javafx.beans.value.{ChangeListener,ObservableValue}
import javafx.scene.layout.{AnchorPane,HBox,VBox,Pane}
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.{ImageView,WritableImage,PixelFormat}
import javafx.scene.shape.{Rectangle}
import javafx.scene.paint.Color
import javafx.event.{ActionEvent, Event, EventHandler}
import javafx.scene.input.{KeyEvent,MouseEvent,ScrollEvent}

//########################################################################
//#    W A T E R F A L L
//########################################################################



class AudioWaterfall(par: App) extends Pane
{

    class Waterfall(width: Double, height: Double) extends Canvas(width, height)
        {
        val iwidth = width.toInt
        val iheight = height.toInt
    
        val img     = new WritableImage(iwidth, iheight)
        val nrPix   = iwidth * iheight
        val pixels  = Array.ofDim[Int](nrPix)
        val lastRow = nrPix - iwidth
        val writer  = img.getPixelWriter
        val format  = PixelFormat.getIntArgbInstance
        val g2d     = getGraphicsContext2D
                
        private val colors = Array.tabulate(256)( i =>
            {
            val prop   = i.toDouble / 256.0
            val hue    = 240.0 - 150.0 * prop
            val bright = 0.1 //  + prop / 2
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
            

        
        /*Scale the power spectrum bins onto the output width.  Do once & reuse. */
        private var pslen = -1
        private var psIndices = Array.fill(iwidth)(0)
        private var psbuf = Array.fill(100)(0)
                
        //only call from javafx thread
        def redraw() =
            {
            if (pslen != psbuf.length)
                {
                pslen = psbuf.length
                psIndices = Array.tabulate(iwidth)(_ * pslen / iwidth)
                }
            val pix = pixels
            System.arraycopy(pix, iwidth, pix, 0, lastRow)
            var pixptr = lastRow
            for (i <- 0 until iwidth)
                {
                val p = psbuf(psIndices(i))
                pix(pixptr) = colors2(p & 0xff)
                pixptr += 1
                }
            //trace("iw:" + iwidth + "  ih:" + iheight + "  pix:" + pix.size + " pslen:" + pslen)
            writer.setPixels(0, 0, iwidth, iheight, format, pix, 0, iwidth)
            g2d.drawImage(img, 0.0, 0.0, width, height)
            }

        def update(ps: Array[Int]) =
            {
            psbuf = ps.clone
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
        private val vscale = 10.0
        private val timeScale = 2
        
        private val g = getGraphicsContext2D
    
        //only call from javafx thread
        def redraw =
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

 
         def update(x: Double, y:Double) = 
            {
            buf(bufPtr) = (x, y)
            bufPtr = (bufPtr + 1) % BUFSIZE
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

    class Redrawer extends EventHandler[ActionEvent]
        {
        override def handle(event: javafx.event.ActionEvent) =
            {
            wf.redraw
            scope.redraw
            }       
        }
        
    val oneFrameAmt = Duration.millis(90);
    var oneFrame = new KeyFrame(oneFrameAmt, new Redrawer)

    TimelineBuilder.create()
       .cycleCount(Animation.INDEFINITE)
       .keyFrames(oneFrame)
       .build()
       .play();

    def update(ps: Array[Int]) =
        wf.update(ps)

    def updateScope(x: Double, y: Double) =
        scope.update(x, y)

}




