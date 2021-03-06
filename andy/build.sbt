import android.Keys._

android.Plugin.androidBuild

name := "bdigi-android"

platformTarget in Android := "android-19"

// call install and run without having to prefix with android:
run <<= run in Android
 
install <<= install in Android


libraryDependencies += "com.google.android" % "support-v4" % "r7"


proguardScala in Android := true

proguardOptions in Android ++= Seq(
    "-dontwarn javax.sound.sampled.**",
    "-dontwarn java.awt.**",
    "-dontwarn junit.**"
)
