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
import android.media.MediaRecorder;

import org.bdigi.*;

class AudioInput implements AudioInputDevice
{
    private AudioRecord input;
    private short buf[];
    private int bufsize;
    private App par;

    /**
     * Convert between   0-32767  <->  0.0-1.0
     */
    private final double doubleToShort  = 32767.0;
    private final double shortToDouble  = 1.0 / 32768.0;

    public AudioInput(App par) {
        this.par    = par;
        int rate    = 44100;
        int config  = AudioFormat.CHANNEL_IN_MONO;
        int format  = AudioFormat.ENCODING_PCM_16BIT;
        bufsize = AudioRecord.getMinBufferSize(rate, config, format);
        if (bufsize < 0) {
            error("Invalid format for this device:" + bufsize);
        } else {
            //DEFAULT or MIC
            //1 = MIC
            input = new AudioRecord(MediaRecorder.AudioSource.DEFAULT,
                 rate, config, format, bufsize);
            if (input.getState() != AudioRecord.STATE_INITIALIZED) {
                error("Not initialized"); 
            }
            buf  = new short[bufsize];
        }
    }


    public void error(String msg) {
        par.error("AudioInput error: " + msg);
    }


    public void trace(String msg) {
        par.trace("AudioInput: " + msg);
    }

    @Override
    public double sampleRate() {
        return 44100.0;
    }

    @Override
    public boolean open()
    {
        input.startRecording();
        return true;
    }

    @Override
    public boolean close() {
        input.stop();
        return true;
    }
    

    @Override
    public scala.Option<double[]> read() {
        int count = input.read(buf, 0, bufsize);
        if (count < 0) {
            //error("read: " + count);
            return scala.Option.apply(null);
            }
        double packet[] = new double[count];
        for (int i= 0 ; i < count ; i++) {
            short iv = buf[i];
            double v = shortToDouble * iv;
            packet[i] = v;
            }
        return scala.Option.apply(packet);
    }


}
