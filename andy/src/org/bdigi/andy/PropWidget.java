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


import android.content.Context;
import android.widget.Toast;
import android.widget.CompoundButton;
import android.widget.FrameLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.TextView;
import android.widget.ToggleButton;
import android.view.View;


import scala.collection.Seq;

import org.bdigi.BooleanProperty;
import org.bdigi.RadioProperty;


class PropWidget
{


    static public class BooleanPropertyWidget extends ToggleButton
    {
        public BooleanPropertyWidget(Context ctx, final BooleanProperty p) {
            super(ctx);
            Boolean checked = (Boolean) p.value();
            setChecked(checked);
            if (p.tooltip().length() > 0) {
                setOnLongClickListener(new OnLongClickListener() {
                    public boolean onLongClick(View v) {
                        Toast.makeText(v.getContext(), p.tooltip(), Toast.LENGTH_SHORT).show();
                        return true;                
                    }
                });
            setOnCheckedChangeListener(new OnCheckedChangeListener() {
                public void onCheckedChanged(CompoundButton button, boolean isChecked) {
                    p.value_$eq(isChecked);
                    }
                });
        
            }
    }//BooleanPropertyWidget    
    
    
    static public class RadioPropertyWidget extends FrameLayout
    {
        public RadioPropertyWidget(Context ctx, final RadioProperty p) {
            super(ctx);
            if (p.tooltip().length() > 0) {
                setOnLongClickListener(new OnLongClickListener() {
                    public boolean onLongClick(View v) {
                        Toast.makeText(v.getContext(), p.tooltip(), Toast.LENGTH_SHORT).show();
                        return true;                
                    }
                });
            }
            TextView lbl = new TextView(ctx);
            lbl.setText(p.label());
            addView(lbl);
            RadioGroup grp = new RadioGroup(ctx);
            grp.setOrientation(RadioGroup.HORIZONTAL);
            addView(grp);
            
            Seq<String> items = p.items();
            
            for (int idx=0 ; idx < items.size() ; idx++) {
            
                String item = items.apply(idx);
                
                RadioButton btn = new RadioButton(ctx);
                btn.setText(item);
                
                class Listener implements View.OnClickListener {
                    int idx;
                    public Listener(int idx) {
                        this.idx = idx;
                        }
                    public void onClick(View v) {
                        p.value_$eq(idx);
                    }
                } 
                btn.setOnClickListener(new Listener(idx));
                grp.addView(btn);
                }
        }
    }//RadioPropertyWidget
    



}













































}
