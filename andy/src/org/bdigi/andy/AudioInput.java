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


class AudioInput implements AudioInputDevice
{
    private int rate;
    private int config;
    private int format;
    private int bufsize;
    private AudioRecord input;
    private short buf[];

    public AudioInput() {
        rate    = 44100;
        config  = AudioFormat.CHANNEL_IN_MONO;
        format  = AudioFormat.ENCODING_PCM_16BIT;
        bufsize = AudioRecord.getMinBufferSize(rate, config, format);
        //0 = MediaRecorder.AudioSource.DEFAULT, tough to import
        input = new AudioRecord(0, rate, config, format, bufsize);
        buf  = new short[bufsize];

    }

    /***
     * Mixin the scala trait Logged
     *
    @Override
    public void error(String msg) {
        Logged$class.error(this, msg);
    }

    @Override
    public void trace(String msg) {
        Logged$class.trace(this, msg);
    }
    */

    public double sampleRate() {
        return 8000.0;
    }

    public boolean open()
    {
        return true;
    }

    public boolean close() {
        return true;
    }

    public scala.Option<double[]> read() {
        return scala.Option.apply(null);
        /*
        var vcount = 0
        val count = input.read(buf, 0, bufsize)
        for (i <- 0 until count)
            {
            val dval = Decimal(buf(i).toInt)
            decimator.update(dval) ( v =>
                {
                vbuf(vcount) = v
                vcount += 1
                })
            }
        val packet = Array.ofDim[Decimal](vcount)
        System.arraycopy(vbuf, 0, packet, 0, vcount)
        Some(packet)
        */
    }


}
