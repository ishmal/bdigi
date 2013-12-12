
package org.bdigi

/**
 * Just add "extends Logged" to any class or trait that needs it
 */ 
object Log
{
    private val className = getClass.getName
    
    private def info =
        {
        val funcName = (new Throwable).getStackTrace()(3).getMethodName
        "[" + className + ":" + funcName + "]"
        }

    def error(msg: String) =
        println(info + " error: " + msg)

    def trace(msg: String) =
        println(info + " : " + msg)
}





