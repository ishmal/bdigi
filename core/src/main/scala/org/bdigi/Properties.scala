/**
 * Scala wrapper of Java properties file.
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2011-2013 Bob Jamison
 * 
 *  This file is part of the Pedro library.
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


import scala.collection.JavaConversions._

class Property[T](val name: String, val label: String, val tooltip: String, val default:T)(changed : (T=>Unit) = {a:T=>})
{
    private var _value : T = default
    
    def value : T = _value
    
    def value_=(v: T) = 
        { 
        val _oldval = _value
        _value = v
        if (_oldval != v) changed(v)
        }
}


class BooleanProperty(name: String, label: String, tooltip: String, default: Boolean = false)(changed: (Boolean=>Unit) = {a:Boolean=>}) 
    extends Property[Boolean](name, label, tooltip, default)(changed)

class IntProperty(name: String, label: String, tooltip: String,  default: Int = 0)(changed: (Int=>Unit) = {a:Int=>}) 
    extends Property[Int](name, label, tooltip, default)(changed)

class DoubleProperty(name: String, label: String, tooltip: String, default: Double = 0.0)(changed: (Double=>Unit) = {a:Double=>}) 
    extends Property[Double](name, label, tooltip, default)(changed)

class StringProperty(name: String, label: String, tooltip: String, default: String="")(changed: (String=>Unit) = {a:String=>}) 
    extends Property[String](name, label, tooltip, default)(changed)

class RadioProperty(name: String, label: String, val items: Seq[String], tooltip: String, default: Int = 0)(changed: (Int=>Unit) = {a:Int=>}) 
    extends Property(name, label, tooltip, default)(changed)

class SelectProperty(name: String, label: String, val items: Seq[String], tooltip: String, default: Int=0)(changed: (Int=>Unit) = {a:Int=>}) 
    extends Property(name, label, tooltip, default)(changed)


case class PropertyGroup(name: String, properties: Property[_]*)
{
    val children : Map[String, Property[_]] = properties.map(p=> (p.name,p)).toMap
}

case class PropertyBundle(name: String, groups: PropertyGroup*)
{

    val children = groups.map(g=> (g.name, g)).toMap
    
    def loadFile(fname: String) : Boolean =
        {
        try
            {
            val file = new java.io.FileInputStream(fname)
            val props = new java.util.Properties
            props.load(file)
            file.close
            val tuples = props.entrySet.map(a => (a.getKey.toString, a.getValue.toString))
            true
            }
        catch
            {
            case e:Exception => Log.error("PropertyBundle.loadFile: " + e)
            false
            }
        }

    def saveFile(fname: String) : Boolean =
        {
        try
            {
            val jprops = new java.util.Properties
            for (gitem <- children)
                {
                val gname = name + "." + gitem._1
                val group = gitem._2
                for (pitem <- group.children)
                    {
                    val pname = gname + "." + pitem._1
                    val prop = pitem._2
                    jprops.put(pname, prop.value.toString)
                    }
                }
            val file = new java.io.FileOutputStream(fname)
            jprops.store(file, "Scala Properties: " + fname)
            file.close
            true
            }
        catch
            {
            case e:Exception => Log.error("PropertyBundle.saveFile: " + e)
            false
            }
        }    





}




/**
 * A very simple Scala wrapper of Properties.  Loads and/or stores a
 * map of name->value.  Note that there is a default value of "", so
 * using a nonexistant name will not cause an exception. 
 */ 
object Properties
{
    def loadFile(fname: String) : Option[Map[String,String]] =
        {
        try
            {
            val file = new java.io.FileInputStream(fname)
            val props = new java.util.Properties
            props.load(file)
            file.close
            val tuples = props.entrySet.map(a => (a.getKey.toString, a.getValue.toString))
            Some(tuples.toMap.withDefaultValue(""))
            }
        catch
            {
            case e:Exception => Log.error("Properties.loadFile: " + e)
            None
            }
        }

    def saveFile(sprops: Map[String,String], fname: String) : Boolean =
        {
        try
            {
            val jprops = new java.util.Properties
            for (a <- sprops) jprops.put(a._1, a._2)
            val file = new java.io.FileOutputStream(fname)
            jprops.store(file, "Scala Properties: " + fname)
            file.close
            true
            }
        catch
            {
            case e:Exception => Log.error("Properties.saveFile: " + e)
            false
            }
        }    

    /*
    def main(argv: Array[String]) =
        {
        val props = Properties.loadFile("props.ini")
        if (props.isEmpty)
            pedro.log.error("Properties test failed")
        else
            {
            val p = props.get
            println(p("a.b.c.d.e"))
            Properties.saveFile(p, "props2.ini")
            }
        }
    */
}



