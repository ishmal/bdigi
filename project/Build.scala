import sbt._
import sbt.Keys._


object MyBuild extends Build {


    override lazy val settings = super.settings ++ Seq(
    
        organization := "org.bdigi",
        version := "1.6",
        scalaVersion := "2.10.3",
        scalacOptions ++= Seq("-deprecation","-feature")
    )
    
    lazy val root = Project(
        id = "bdigi",
        base = file(".") 
    ) aggregate (core, fx, andy)

    lazy val core = Project(
        id = "core",
        base = file("core")
    )

    lazy val fx = Project(
        id = "fx",
        base = file("fx")
    ) dependsOn core


    lazy val andy = Project(
        id = "andy",
        base = file("andy")
    ) dependsOn core


}//MyBuild



