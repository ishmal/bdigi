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
import javafx.scene.layout.{AnchorPane, BorderPane, HBox,VBox,Pane}
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.{ImageView,WritableImage,PixelFormat}
import javafx.scene.shape.{Rectangle}
import javafx.scene.paint.Color
import javafx.event.{ActionEvent, Event, EventHandler}
import javafx.scene.input.{KeyEvent,MouseEvent,ScrollEvent}

//########################################################################
//#    Tuning Panel.    A single canvas with three widgets
//########################################################################



class TuningPanel(par: App) extends AnchorPane
{


    class WaterfallArea(width: Int, height: Int)
        {    
        val img     = new WritableImage(width, height)
        val nrPix   = width * height
        val pixels  = Array.ofDim[Int](nrPix)
        val lastRow = nrPix - width
        val writer  = img.getPixelWriter
        val format  = PixelFormat.getIntArgbInstance
                
        private val colors = Array.tabulate(256)( i =>
            {
            val prop   = i.toDouble / 256.0
            val hue    = 240.0 - 150.0 * prop
            val bright = 0.3 + prop / 2
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
        private var psIndices = Array.fill(width)(0)
        private var psbuf = Array.fill(100)(0)
                
        //only call from javafx thread
        def redraw() =
            {
            if (pslen != psbuf.length)
                {
                pslen = psbuf.length
                psIndices = Array.tabulate(width)(_ * pslen / width)
                }
            val pix = pixels
            System.arraycopy(pix, width, pix, 0, lastRow)
            var pixptr = lastRow
            for (i <- 0 until width)
                {
                val p = psbuf(psIndices(i))
                pix(pixptr) = colors2(p & 0xff)
                pixptr += 1
                }
            //trace("iw:" + iwidth + "  ih:" + iheight + "  pix:" + pix.size + " pslen:" + pslen)
            writer.setPixels(0, 0, width, height, format, pix, 0, width)
            ctx.drawImage(img, 0.0, 0.0, width, height)
            }

        def update(ps: Array[Int]) =
            {
            psbuf = ps.clone
            }
                
        }//Waterfall



    class TunerArea(width: Int, height: Int)
        {
        val range = maxFreq - minFreq
            
        def freq = par.getFrequency
        def freq_=(v: Double) =
            {
            par.setFrequency(v)
            }
            
        def bw = par.mode.bandwidth
            
        def redraw = 
            {
            val top = getHeight.toInt - height
            
            ctx.setFill(Color.BLACK)
            ctx.fillRect(0, top, width.toInt, height.toInt)
            
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
                    ctx.setFill(Color.GREEN)
                    ctx.fillRect(hx, top, 2.0, 10.0)
                    val str = "%d".format(tick)
                    ctx.setFill(Color.CYAN)
                    ctx.fillText(str, hx-16.0, top+19.0)
                    }
                else if (tick % 100 == 0)
                    {
                    ctx.setFill(Color.GREEN)
                    ctx.fillRect(hx, top, 2.0, 5.0)
                    }
                else
                    {
                    ctx.setFill(Color.GREEN)
                    ctx.fillRect(hx, top, 2.0, 2.0)
                    }
                }
    
            
            ctx.setFill(Color.GREEN)
            val fx = width * freq / range
            ctx.fillRect(fx, 3, 1.0, getHeight-10)
            
            if (par.bandwidth > 0.0)
                {
                ctx.setFill(Color.RED)
                val lox = width * (freq - bw * 0.5) / range
                ctx.fillRect(lox, top+5, 1.0, 10)
                val hix = width * (freq + bw * 0.5) / range
                ctx.fillRect(hix, top+5, 1.0, 10)
                }
            }
            
        
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
                    case MouseEvent.MOUSE_CLICKED => freq = x2freq(evt.getX)
                    case MouseEvent.MOUSE_DRAGGED => freq = x2freq(evt.getX)
                    case _ =>
                    }
                }
            }) 
            
        setOnScroll(new EventHandler[ScrollEvent]
            {
            def handle(evt: ScrollEvent)
                {
                freq =  if (evt.getDeltaY > 0) freq + 1 else freq - 1 
                redraw
                }
            });
        
        }//Tuner
    

    class ScopeArea(width: Int, height: Int) extends Canvas(width, height)
        {
        private val BUFSIZE = 512
        private val buf = Array.fill(BUFSIZE)((0.0,0.0))
        private var bufPtr = 0
        private var lastx = 0.0
        private var lasty = 0.0
        private val vscale = 10.0
        private val timeScale = 2
        val ctx = canvas.getGraphicsContext2D
        
        //only call from javafx thread
        def redraw =
            {
            val w   = width
            val w2  = w * 0.5
            val h   = height
            val h2  = h * 0.5
            val x0  = w2
            val y0  = h2
            var ptr = bufPtr
            var x   = 0.0
            var y   = 0.0

            //crosshairs
            ctx.setFill(Color.BLACK)
            ctx.fillRect(0, 0, w, h)
            ctx.setStroke(Color.WHITE)
            ctx.strokeLine(0, h2,  w, h2)
            ctx.strokeLine(w2,  0, w2,  h)

            //the trace line
            ctx.setStroke(Color.YELLOW)
            for (i <- 0 until BUFSIZE by timeScale)
                {
                val v = buf(ptr)
                val vx = v._1
                val vy = v._2
                ptr = (ptr + 1) % BUFSIZE
                x = x0 + vx * vscale
                y = y0 + vy * vscale
                ctx.strokeLine(lastx, lasty, x, y)
                lastx = x
                lasty = y
                }
            ctx.setFill(Color.RED)
            ctx.fillRect(x-2.0, y-2.0, 4.0, 4.0)
            }    

 
         def update(x: Double, y:Double) = 
            {
            buf(bufPtr) = (x, y)
            bufPtr = (bufPtr + 1) % BUFSIZE
            }
            
                  
    } //scope
    
    def trace(msg: String) =
        par.trace(msg)
    
    def error(msg: String) =
        par.error(msg)
    
    AnchorPane.setLeftAnchor(this, 0)
    AnchorPane.setTopAnchor(this, 0)
    AnchorPane.setRightAnchor(this, 0)
    AnchorPane.setBottomAnchor(this, 0)

    
    val minFreq = 0.0
    val maxFreq = 2500.0
    
    
    private val initialWidth = 500
    private val initialHeight = 100
    private val TUNER_HEIGHT = 24

    val canvas = new Canvas(initialWidth, initialHeight)
    AnchorPane.setLeftAnchor(canvas, 0)
    AnchorPane.setTopAnchor(canvas, 0)
    AnchorPane.setRightAnchor(canvas, 0)
    AnchorPane.setBottomAnchor(canvas, 0)
    getChildren.add(canvas)
    val ctx = canvas.getGraphicsContext2D

    var waterfall = new WaterfallArea(initialWidth, initialHeight - TUNER_HEIGHT)
    var tuner     = new TunerArea(initialWidth, TUNER_HEIGHT)
    var scope     = new ScopeArea(initialHeight - TUNER_HEIGHT, initialHeight - TUNER_HEIGHT)

    private val resizeListener = new ChangeListener[Number]
        {
        override def changed(observableValue: ObservableValue[_ <: Number], oldval: Number, newval: Number)
            {
            val w = if (getWidth != 0) getWidth.toInt else 50
            val h = if (getHeight != 0) getHeight.toInt else 50
            println("w: " + w + "  h:" + h)
            canvas.setWidth(w)
            canvas.setHeight(h)
            waterfall = new WaterfallArea(w, h - TUNER_HEIGHT)
            tuner     = new TunerArea(w, TUNER_HEIGHT)
            scope     = new ScopeArea(h - TUNER_HEIGHT, h - TUNER_HEIGHT)
            }
        }
    
    widthProperty.addListener(resizeListener)
    heightProperty.addListener(resizeListener)

    class Redrawer extends EventHandler[ActionEvent]
        {
        override def handle(event: javafx.event.ActionEvent) =
            {
            waterfall.redraw
            tuner.redraw
            if (showScope)
                scope.redraw
            }       
        }
        
    val oneFrameAmt = Duration.millis(70);
    var oneFrame = new KeyFrame(oneFrameAmt, new Redrawer)

    TimelineBuilder.create()
       .cycleCount(Animation.INDEFINITE)
       .keyFrames(oneFrame)
       .build()
       .play();

    def update(ps: Array[Int]) =
        waterfall.update(ps)

    def updateScope(x: Double, y: Double) =
        scope.update(x, y)
        
    var showScope = true

}




