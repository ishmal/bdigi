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
    private double vbuf[];
    private FirResampler resampler;
    private App par;

    /**
     * Convert between   0-32767  <->  0.0-1.0
     */
    private final double doubleToShort  = 32767.0;
    private final double shortToDouble  = 1.0 / 32768.0;

    public AudioInput(App par) {
        this.par = par;
        rate    = 44100;
        config  = AudioFormat.CHANNEL_IN_MONO;
        format  = AudioFormat.ENCODING_PCM_16BIT;
        bufsize = AudioRecord.getMinBufferSize(rate, config, format);
        //0 = MediaRecorder.AudioSource.DEFAULT, tough to import
        input = new AudioRecord(0, rate, config, format, bufsize);
        buf  = new short[bufsize];
        vbuf = new double[bufsize];
        resampler = new FirResampler(6);

    }


    public void error(String msg) {
        par.error("AudioInput error: " + msg);
    }


    public void trace(String msg) {
        par.trace("AudioInput: " + msg);
    }

    @Override
    public double sampleRate() {
        return 7350.0;
    }

    @Override
    public boolean open()
    {
        return true;
    }

    @Override
    public boolean close() {
        return true;
    }
    

    @Override
    public scala.Option<double[]> read() {
        int count = input.read(buf, 0, bufsize);
        if (count < 0)
            return scala.Option.apply(null);
        for (int i=0 ; i < count ; i++)
            vbuf[i] = shortToDouble * buf[i];        
        double packet[] = resampler.decimate(vbuf, count);
        return scala.Option.apply(packet);
    }


}
