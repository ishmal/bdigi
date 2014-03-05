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


package org.bdigi.fx


import javafx.application.{Application, Platform}
import javafx.beans.value.{ChangeListener,ObservableValue}
import javafx.geometry.{HPos,VPos}
import javafx.stage.{Stage,WindowEvent}
import javafx.collections.{FXCollections,ObservableList}
import javafx.fxml.{FXML,FXMLLoader}
import javafx.scene.{Node=>jfxNode, Parent, Scene}
import javafx.scene.control.{Button,CheckBox,ChoiceBox,Tab,TabPane,TextArea,TextField,ToggleButton,Tooltip}
import javafx.event.{ActionEvent,Event,EventHandler}
import javafx.scene.layout.{AnchorPane,FlowPane,HBox,VBox,VBoxBuilder,Pane}
import javafx.scene.input.{KeyEvent,KeyCode}
import javafx.scene.paint.Color
import javafx.scene.image.Image
import javafx.scene.text.Text
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

import scala.collection.JavaConversions._

import org.bdigi._

class Console2(par: App) extends TextArea
{
    setEditable(false)
    
    private var autoAdjust = true
    
    val queue = scala.collection.mutable.Queue[String]()
    
    private var busy = false
    val refresher = new Runnable
        {
        override def run =
            {
            try
                {
                while (!queue.isEmpty)
                    {
                    appendText(queue.dequeue)
                    if (autoAdjust)
                        positionCaret(getText.length)
                    }
                }
            catch
                {
                case e: Exception => par.error("Console 2: ", e)
                  e.printStackTrace
                }
            }
        }
    
    def puttext(str: String) =
        {
        queue += str
        Platform.runLater(refresher)
        }
        
    override def clear =
        {
        queue.clear
        super.clear
        }
        
    addEventHandler(KeyEvent.KEY_PRESSED, new EventHandler[KeyEvent]
        {
        def handle(evt: KeyEvent)
            {
            if (evt.getCode == KeyCode.SPACE)
                autoAdjust = !autoAdjust
            }
        })
}



class InputText2(par: App, rows: Int, cols: Int) extends TextArea
{
    setPrefRowCount(rows)
    setPrefColumnCount(cols)
    setEditable(true)
    setWrapText(true)
    
    var lastPos = 0
    
    override def clear =
        {
        super.clear
        lastPos = 0
        }
    
    def gettext =
        {
        val text = getText
        val len = text.length
        val res = if (lastPos < len)
            text.substring(lastPos)
        else
            ""
        lastPos = len
        res
        }
}

class LogDialog2(par: App) extends Stage
{
    setTitle("Log")
    val vbox = new VBox
    val console = new Console2(par)
    vbox.getChildren().addAll(console)
    val scene = new Scene(vbox)
    setScene(scene)
    
    def puttext(str: String) =
        {
        console.puttext(str)
        console.puttext("\n")
        }
    
}

class PrefsDialog(par: App) extends Stage
{

    @FXML var callField       : TextField = _
    @FXML var nameField       : TextField = _
    @FXML var locatorField    : TextField = _
    @FXML var inputDeviceList : ChoiceBox[String] = _
    @FXML var outputDeviceList: ChoiceBox[String] = _
    
    private def seqToFxList(xs: Map[String, AudioDeviceInfo]) =
        {
        val list = FXCollections.observableArrayList[String]()
        for (dev <- xs)
            list.add(dev._1)
        list
        }

    @FXML def initialize =
        {
        callField.setText(par.config.call)
        nameField.setText(par.config.name)
        locatorField.setText(par.config.locator)
        inputDeviceList.setItems(seqToFxList(AudioDevice.inputDevices))
        inputDeviceList.setValue(par.config.audioInputDevice)
        outputDeviceList.setItems(seqToFxList(AudioDevice.outputDevices))
        outputDeviceList.setValue(par.config.audioOutputDevice)
        
        /*
        inputDeviceList.valueProperty.addListener(new ChangeListener[String]
            {
            override def changed(ov: ObservableValue[_ <: String], t: String, t1: String)
                {       
                val name = ov.getValue         
                println("Selected: " + name) 
                par.setInputDevice(name)               
                }    
            })
        */
        }
        
    @FXML def doOk(evt: Event) =
        {
        par.config.call    = callField.getText
        par.config.name    = nameField.getText
        par.config.locator = locatorField.getText
        var inp            = inputDeviceList.getValue
        if (inp != par.config.audioInputDevice)
            {
            par.setInputDevice(inp) 
            }
        par.config.audioInputDevice = inp
        var outp            = outputDeviceList.getValue
        if (outp != par.config.audioOutputDevice)
            {
            par.setOutputDevice(outp) 
            }
        par.config.audioOutputDevice = outp
        par.configSave
        close
        }

    @FXML def doCancel(evt: Event) =
        {
        close
        }

    try
        {
        val loader = new FXMLLoader(getClass.getResource("/prefs.fxml"))
        loader.setController(this)
        loader.load
        val scene = new Scene(loader.getRoot.asInstanceOf[Parent])
        setTitle("Preferences")
        setScene(scene)
        }
    catch
        {
        case e : Exception =>  e.printStackTrace
        }
}











class MainController(stage: Stage) extends App
{
    
    @FXML var waterfallBox : AnchorPane = _
    val audioWaterfall = new AudioWaterfall(this)
    
    @FXML var modePane : TabPane = _
    
    
    @FXML var consoleTextBox : VBox = _
    val consoleText = new Console2(this)

    @FXML var inputTextBox : VBox = _
    val inputText = new InputText2(this, 20, 80)
    
    val aboutDialog = new Stage
        {
        val root = FXMLLoader.load(getClass.getResource("/about.fxml")).asInstanceOf[Parent]
        setTitle("About ScalaDigi")
        setScene(new Scene(root))
        }
        
    val prefsDialog = new PrefsDialog(this)
        
    val logDialog = new LogDialog2(this)
    logDialog.puttext("Hello, world")
    
    stage.setOnCloseRequest(new EventHandler[WindowEvent]
        {
        override def handle(evt: WindowEvent) 
            {
            doClose(evt)
            }
        })
        
        
    
    def doClose      (evt : Event) = { stopProcessing ; Platform.exit }
    def doClear      (evt : Event) = { consoleText.clear ; inputText.clear }
    def doLog        (evt : Event) = logDialog.show
    def doAbout      (evt : Event) = aboutDialog.show
    def doPreferences(evt : Event) = prefsDialog.show
    def doRxTx       (evt : Event) = rxtx = evt.getSource.asInstanceOf[ToggleButton].isSelected
    def doAgc        (evt : Event) = agc = evt.getSource.asInstanceOf[ToggleButton].isSelected
    
    
    
    /**
     * Called by the FXMLLoader as a place to do post-loading setup
     */
    @FXML def initialize =
        {
        waterfallBox.getChildren.add(audioWaterfall)
        consoleTextBox.getChildren.add(consoleText)
        inputTextBox.getChildren.add(inputText)  
        
        for (mode <- modes)
            {
            val tab = new Tab(mode.name)
            tab.setTooltip(new Tooltip(mode.tooltip))
            modePane.getTabs.add(tab)
            val pane = new FlowPane
            tab.setContent(pane)
            tab.setOnSelectionChanged(new EventHandler[Event]
                {
                override def handle(evt: Event)
                    {
                    self.mode = mode //set parent to this mode
                    }
                })
            
            for (prop <- mode.properties.properties)
                {
                prop match
                     {
                     case p : BooleanProperty =>
                         pane.getChildren.add(new BooleanPropertyWidget(p))
                     case p : RadioProperty =>
                         pane.getChildren.add(new RadioPropertyWidget(p))
                     case _ =>
                     }
                }
            } 
        }
        
        
    /**
     * Override these in your client code, especially for a GUI
     */

    override def gettext : String =
        {
        if (inputText != null)
            inputText.gettext
        else
            ""
        }

    override def puttext(msg: String)  =
        {
        if (consoleText != null)
            consoleText.puttext(msg)
        }

    override def status(msg: String)  =
        {
        if (logDialog != null)
            logDialog.puttext(msg + "\n")
        }

    override def updateSpectrum(ps:  Array[Int]) =
        {
        if (audioWaterfall != null)
            audioWaterfall.update(ps)
        }
    
    override def adjust =
        {
        
        }

    override def updateScope(x: Double, y: Double) =
        {
        if (audioWaterfall != null)
            audioWaterfall.updateScope(x, y)
        }

    startProcessing        
}


class Main extends Application
{
    
    override def start(stage: Stage) =
        {
        try
            {
            val controller = new MainController(stage)
            val loader = new FXMLLoader(getClass.getResource("/main.fxml"))
            loader.setController(controller)
            val page = loader.load.asInstanceOf[Parent]
            val scene = new Scene(page)
            stage.setTitle("bdigi")
            stage.getIcons.add(new Image(getClass.getResourceAsStream("/icon.png")));
            stage.setScene(scene)
            stage.show
            }
        catch
            {
            case e: java.io.IOException => println("error:" + e)
                                e.printStackTrace
            }
        }     
}


object Main
{
    def main(argv: Array[String]) : Unit =
        {
        javafx.application.Application.launch(classOf[Main], argv:_*)
        }
}











