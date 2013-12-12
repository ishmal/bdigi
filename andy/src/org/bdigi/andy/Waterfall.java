package org.bdigi.andy;


import android.graphics.Bitmap;
import android.graphics.Canvas;

import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.widget.FrameLayout;


class Waterfall extends FrameLayout
{
    private MainActivity par;
    private DrawArea drawArea;

    public Waterfall(MainActivity par) {
        super(par);
        this.par = par;
        drawArea = new DrawArea(100,100);
        }

class DrawArea extends SurfaceView implements SurfaceHolder.Callback
{

    private int width;
    private int height;
    private int size;
    private Bitmap img;
    private int pixels[];
    private int colors[];
    private int bins;
    /*Scale the power spectrum bins onto the output width.  Do once & reuse. */
    private int psIndices[];

    public DrawArea(int width, int height) {
        super(par);
        this.width = width;
        this.height = height;
        size = width * height;
        colors = makeColors();
        pixels = new int[size];
        img = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
        bins = 300;
        for (int i= 0 ; i < width ; i++)
            psIndices[i] = i * bins / width;
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




	


	
	private void updateBuffer(double ps[]) {
		/**
		 * First scroll the image up one pixel
		 */
		int yp = size - width;  //first pixel of last row
		System.arraycopy(pixels, width, pixels, 0, yp); //shift up one row

		/**
		 * Now calculate and plot the new data into the bottom row
		 */
        for (int x = 0 ; x < width ; x++) {
        	double v = ps[psIndices[x]];
        	double scaledv = Math.log(v) * 60.0;
        	int clridx = ((int)scaledv) & 0xff;
        	pixels[yp] = colors[clridx];
        	}
	    }
        

	/**
	 * Called from another thread
	 * @param arr an array of doubles describing the power spectrum to display
	 */
    public void update(double arr[]) {
        SurfaceHolder holder = getHolder();
        Canvas c = holder.lockCanvas(null);
        updateBuffer(arr);
        img.setPixels(pixels, 0, width, 0, 0, width, height);
        c.drawBitmap(img, 0f, 0f, null);
        holder.unlockCanvasAndPost(c);    
        }

	@Override
    public void surfaceChanged(SurfaceHolder holder, int format, int width, int height)
        {
	    }

	@Override public void surfaceCreated(SurfaceHolder holder)
	    {
	    }

	@Override public void surfaceDestroyed(SurfaceHolder holder)
	    {
	    }
	        					
} //DrawArea


    @Override public void onSizeChanged(int x, int y, int oldx, int oldy)
        {
        drawArea = new DrawArea(x,y);
        }

    public void update(double arr[])
        {
        drawArea.update(arr);
        }


}//Waterfall

