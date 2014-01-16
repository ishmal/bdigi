/**
 * Scala SDR tool
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (c) 2014 Bob Jamison
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

package org.bdigi

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,Matchers}



@RunWith(classOf[JUnitRunner])
class MainTest extends FeatureSpec with Matchers
{
/* redo this completely when we have time
    feature("Assorted high level functionality")
        {
  
        scenario("Test 1")
            {
            val me = new App
                {
                override val receiver = new Actor
                    {
                    def act
                        {
                        var keepGoing = true
                        loopWhile (keepGoing)
                            {
                            receive
                                {
                                case Terminate =>
                                    keepGoing = false
                                case fall: sdr.gui.Waterfall =>
                                    info("received waterfall")
                                }
                            }
                        }
                    }
                }
            info("hello")
            val audio = new JavaSoundAudio(me)
            me.receiver.start
            audio.start
            audio ! AudioStart
            Thread.sleep(5000)
            audio ! AudioStop
            audio ! Terminate
            me.receiver    ! Terminate
            }

        }
*/  

}

