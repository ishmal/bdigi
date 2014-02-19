import sbt._
import sbt.Keys._
import sbtassembly.Plugin._ 
import AssemblyKeys._

object MyBuild extends Build {


    lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    
        organization := "org.bdigi",
        version := "1.6",
        scalaVersion := "2.10.3",
        scalacOptions ++= Seq("-deprecation","-feature")
    )
    
    lazy val root = Project(
        id = "bdigi",
        base = file("."),
        settings = buildSettings
        
    ) aggregate (core, fx, andy)

    lazy val core = Project(
        id = "core",
        base = file("core"),
        settings = buildSettings
    )

    lazy val fx = Project(
        id = "fx",
        base = file("fx"),
        settings = buildSettings ++ assemblySettings
    ) dependsOn core


    lazy val andy = Project(
        id = "andy",
        base = file("andy"),
        settings = buildSettings
    ) dependsOn core


}//MyBuild



