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


package org.bdigi.andy;


import android.graphics.Bitmap;
import android.graphics.Canvas;

import android.content.Context;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.widget.LinearLayout;
import android.os.Handler;
import android.util.AttributeSet;

class Waterfall extends SurfaceView implements SurfaceHolder.Callback
{

    private int imgWidth;
    private int imgHeight;
    private int imgSize;
    private Bitmap img;
    private int pixels[];
    private int colors[];
    private int bins;
    /*Scale the power spectrum bins onto the output width.  Do once & reuse. */
    private int psbuf[];
    private int pslen;
    private int psIndices[];

    private MainActivity par;
    private Animator animator;

    public void error(String msg) {
        par.error("Waterfall: " + msg);
    }

    public void trace(String msg) {
        par.trace("Waterfall: " + msg);
    }
    
    public Waterfall(Context par, AttributeSet attrs) {
        super(par, attrs);
        this.par = (MainActivity) par;
        trace("i got called");
        colors = makeColors();
        setup();
        setBackgroundColor(0xff00ff00);
        setMinimumHeight(100);
        invalidate();
        animator = new Animator(100);
        animator.start();
        psbuf = new int[500];
        pslen = -1;
        setLayoutParams(new LinearLayout.LayoutParams(
                         LinearLayout.LayoutParams.FILL_PARENT,
                         LinearLayout.LayoutParams.FILL_PARENT));
    }
    
    private void setup() {
        imgWidth  = getWidth();
        if (imgWidth <= 0) imgWidth = 10;
        imgHeight = getHeight();
        if (imgHeight <= 0) imgHeight = 10;
        imgSize   = imgWidth * imgHeight;
        pixels    = new int[imgSize];
        img       = Bitmap.createBitmap(imgWidth, imgHeight, Bitmap.Config.ARGB_8888);
        bins      = 300;
        psIndices = new int[imgWidth];
        for (int i= 0 ; i < imgWidth ; i++)
            psIndices[i] = i * bins / imgWidth;
        getHolder().addCallback(this);
    
    }


    private int[] makeColors() {
        int colors[] = new int[256];
        for (int i = 0 ; i < 256 ; i++) {
            int r = (i < 170) ? 0 : (i-170) * 3;
            int g = (i <  85) ? 0 : (i < 170) ? (i-85) * 3 : 255;
            int b = (i <  85) ? i * 3 : 255;
            int col = 0xff;
            col = (col << 8) + r;
            col = (col << 8) + g;
            col = (col << 8) + b;
            colors[i] = col;

        }
        return colors;
    }
    
	
	private void redrawImage() {
	    //if power spectrum length changes, we need to recalc
	    if (pslen != psbuf.length) {
	        pslen = psbuf.length;
	        for (int i = 0 ; i < imgWidth ; i++) {
	            psIndices[i] = i * pslen / imgWidth;
	        }    
	    }
	    
		/**
		 * First scroll the image up one pixel
		 */
		int yp = imgSize - imgWidth;  //first pixel of last row
		System.arraycopy(pixels, imgWidth, pixels, 0, yp); //shift up one row

		/**
		 * Now calculate and plot the new data into the bottom row
		 */
        for (int x = 0 ; x < imgWidth ; x++) {
        	int v      = psbuf[psIndices[x]];
        	int clridx = v & 0xff;
        	pixels[yp] = colors[clridx];
        	}
	    }
        

	/**
	 * Called from another thread
	 * @param arr an array of doubles describing the power spectrum to display
	 */
    @Override 
    public void onDraw(Canvas c) {
        super.onDraw(c);
        //trace("redraw:");
        redrawImage();
        img.setPixels(pixels, 0, imgWidth, 0, 0, imgWidth, imgHeight);
        c.drawBitmap(img, 0f, 0f, null);
        }

    public void update(int ps[]) {
        psbuf = ps.clone();
        }

	@Override 
	public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
        {
	    }

	@Override 
	public void surfaceCreated(SurfaceHolder holder)
	    {
	    }

	@Override 
	public void surfaceDestroyed(SurfaceHolder holder)
	    {
	    }
	        					
    @Override 
    public void onSizeChanged(int x, int y, int oldx, int oldy)
        {
        trace("onsizechanged: " + x + " ," + y);
        trace("w: " + getWidth() + " h: " + getHeight());
        setup();
        }

    class Animator implements Runnable
    {
        private Handler handler;
        private int period;
        private boolean cont;
    
        public Animator(int millis) {
            this.handler = new Handler();
            this.period = millis;
        }

        public void start() {
            cont = true;
            handler.postDelayed(this, period);
        }

        public void stop() {
            cont = false;
        }


        @Override
        public void run() {
            invalidate();
            if (cont)
                handler.postDelayed(this, period);
        }

    }//Animator


}//Waterfall

