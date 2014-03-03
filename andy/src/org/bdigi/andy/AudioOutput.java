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
    private int bufsize;
    private AudioTrack output;
    private short[] sbuf;
    private int sptr;
    private int slen;
    private App par;

    /**
     * Convert between   0-32767  <->  0.0-1.0
     */
    private final double doubleToShort  = 32767.0;
    private final double shortToDouble  = 1.0 / 32768.0;


    public AudioOutput(App par) {
        this.par = par;
        int rate = 44100;
        int streamtype = AudioManager.STREAM_MUSIC;
        int config = AudioFormat.CHANNEL_OUT_MONO;
        int format = AudioFormat.ENCODING_PCM_16BIT;
        bufsize = AudioTrack.getMinBufferSize(rate, config, format);
        int mode = AudioTrack.MODE_STREAM;
        output = new AudioTrack(streamtype, rate, config, format, bufsize, mode);
        sbuf  = new short[bufsize];
        slen = bufsize;
    }

    public void error(String msg) {
        par.error("AudioOutput error: " + msg);
    }


    public void trace(String msg) {
        par.trace("AudioOutput: " +  msg);
    }

    public double sampleRate() {
        return 44100.0;
    }
    
    public boolean write(double inbuf[]) {
        int len = inbuf.length;
        for (int i=0 ; i < len ; i++)
            {
            sbuf[sptr++] = (short) (doubleToShort * inbuf[i]);
            if (sptr >= slen)
                {
                output.write(sbuf, 0, slen);
                sptr = 0;
                }
            }
        return true;
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
