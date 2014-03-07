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


import javafx.scene.control.{Button,CheckBox,ChoiceBox,Label,RadioButton,
    Tab,TabPane,TextArea,TextField,ToggleButton,ToggleGroup,Tooltip}
import javafx.event.{ActionEvent,Event,EventHandler}
import javafx.scene.layout.{AnchorPane,FlowPane,HBox,VBox}

import org.bdigi.{BooleanProperty,RadioProperty}



class BooleanPropertyWidget(p: BooleanProperty) extends ToggleButton(p.label)
{
    setSelected(p.value)
    if (p.tooltip.size > 0)
        setTooltip(new Tooltip(p.tooltip))
    setOnAction(new EventHandler[ActionEvent]
		 {
		 override def handle(evt: ActionEvent)
			 {
			 p.value = isSelected
			 }
		 })
}


class RadioPropertyWidget(p: RadioProperty) extends VBox
{
    getStyleClass().add("radiogroup");
    val hbox = new HBox
    getChildren.addAll(new Label(p.label), hbox)
    val group = new ToggleGroup
    
    for (idx <- 0 until p.items.size)
        {
        val btn = new RadioButton(p.items(idx))
        btn.setToggleGroup(group)
        hbox.getChildren.add(btn)
        if (idx == p.value)
            btn.setSelected(true)
        if (p.tooltip.size > 0)
            btn.setTooltip(new Tooltip(p.tooltip))
        btn.setOnAction(new EventHandler[ActionEvent]
             {
             val index = idx
             override def handle(evt: ActionEvent)
                 {
                 p.value = index
                 }
             })
        }
    
}
