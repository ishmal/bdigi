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

import android.media.AudioFormat;
import android.media.AudioManager;
import android.media.AudioRecord;
import android.media.AudioTrack;

import org.bdigi.*;



class AudioOutput implements AudioOutputDevice
{
    private int rate;
    private int streamtype;
    private int config;
    private int format;
    private int bufsize;
    private int mode;
    private AudioTrack output;
    private short[] buf;

    /***
     * Mixin the scala trait Logged

    @Override
    public void error(String msg) {
        Logged$class.error(this, msg);
    }

    @Override
    public void trace(String msg) {
        Logged$class.trace(this, msg);
    }
    */

    public AudioOutput() {
        rate = 44100;
        streamtype = AudioManager.STREAM_MUSIC;
        config = AudioFormat.CHANNEL_IN_MONO;
        format = AudioFormat.ENCODING_PCM_16BIT;
        bufsize = AudioTrack.getMinBufferSize(rate, config, format);
        mode = AudioTrack.MODE_STREAM;
        output = new AudioTrack(streamtype, rate, config, format, bufsize, mode);
        buf  = new short[bufsize];
    }

    public double sampleRate() {
        return 8000.0;
    }
    
    public boolean write(double inbuf[]) {
        return false;
        /*
        var bufptr = 0
        for (iv <- inbuf)
            {
            interpolator.update(iv)( v =>
                {
                val sval = v.v.toShort
                buf(bufptr) = sval
                bufptr += 1
                if (bufptr >= bufsize)
                    {
                    output.write(buf, 0, bufsize)
                    bufptr += 1
                    }
                })
            }
        output.write(buf, 0, bufptr)
        true
        */
        }

    public boolean open()
        {
        return true;
        }
        
    public boolean close()
        {
        output.release();
        return true;
        }

}
