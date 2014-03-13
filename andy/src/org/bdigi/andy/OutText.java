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


import android.widget.TextView;
import android.os.Handler;
import android.content.Context;
import android.util.AttributeSet;
import android.text.method.ScrollingMovementMethod;
import java.util.concurrent.ConcurrentLinkedQueue;


/**
 * This small class implements a scrolling output text
 */
class OutText extends TextView
{

    private Handler handler;    
    private ConcurrentLinkedQueue<String> queue;
    private boolean autoScroll;

    public OutText(Context ctx, AttributeSet attrs) {
        super(ctx, attrs);
        setMovementMethod(new ScrollingMovementMethod());
        queue = new ConcurrentLinkedQueue<String>();
        handler = new Handler();
        autoScroll = true;
    }
    
    private void doScroll() {
        if (!autoScroll)
            return;
        int scrollAmount = getLayout().getLineTop(getLineCount()) - getHeight();
        if (scrollAmount > 0)
            scrollTo(0, scrollAmount);
        else
            scrollTo(0, 0);
    }

    public void puttext(final String txt) {
    
        queue.add(txt);
        
        handler.post(new Runnable() {
        @Override
        public void run() {
            while (!queue.isEmpty())
                append(queue.remove());
            doScroll();
            }
        });    
    }

    public void clear() {
    
        handler.post(new Runnable() {
        @Override
        public void run() {
            queue.clear();
            setText("");
            }
        });  

    }



}
