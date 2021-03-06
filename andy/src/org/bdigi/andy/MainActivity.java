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

import android.app.Activity;
import android.os.Bundle;

import java.util.ArrayList;
import java.util.List;
import java.io.FileInputStream;
import java.io.FileOutputStream;


import android.os.Bundle;
import android.content.Context;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.SimpleOnPageChangeListener;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.ToggleButton;
import android.widget.LinearLayout;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import org.bdigi.*;

import org.bdigi.mode.Mode;
import scala.collection.Seq;

public class MainActivity extends FragmentActivity {


    public class MainApp extends App {

        //########################################
        //# Logging
        //########################################
    
        @Override
        public void trace(String msg)
            {
            MainActivity.this.trace(msg);
            }

        @Override
        public void error(String msg)
            {
            MainActivity.this.error(msg);
            }

        @Override
        public void error(String msg, Throwable e) 
            {
            MainActivity.this.error(msg, e);
            }
    

        //########################################
        //# Audio I/O
        //########################################
    
        @Override
        public void setInputDevice(String deviceName)
            {
            AudioInputDevice input = (AudioInputDevice)new org.bdigi.andy.AudioInput(this);
            input.open();
            inputDevice_$eq(scala.Option.apply(input));
            }
    
        @Override
        public void setOutputDevice(String deviceName)
            {
            AudioOutputDevice output = (AudioOutputDevice)new org.bdigi.andy.AudioOutput(this);
            output.open();
            outputDevice_$eq(scala.Option.apply(output));
            }
    
  
    
        //########################################
        //# Config
        //########################################
        @Override
        public boolean configLoad()
            {
            try
                {
                FileInputStream ins = openFileInput("bdigi.ini");
                config().load(ins);
                ins.close();
                return true;
                }
            catch (Exception e)
                {
                error("configLoad failed: " + e);
                return false;
                }
            }

        @Override
        public boolean configSave()
            {
            try
                {
                FileOutputStream outs = openFileOutput("bdigi.ini", Context.MODE_PRIVATE);
                config().save(outs);
                outs.close();
                return true;
                }
            catch (Exception e)
                {
                error("configSave failed: " + e);
                return false;
                }
            }


        /**
         * Override these in your client code, especially for a GUI
         */

        @Override
        public void status(String msg)
            {
            }
    
        @Override
        public void puttext(String msg)
            {
            if (outText != null)
                outText.puttext(msg);
            }
    
        @Override
        public String gettext()
            {
            return "";
            }
        
        @Override
        public void updateScope(double x, double y)
            {
            }

        @Override
        public void updateSpectrum(int ps[])
            {
            if (waterfall != null)
                waterfall.update(ps);
            else
                trace("no waterfall");
            }
    
        @Override
        public void adjust()
            {
            }
        
        public MainApp()
            {
            super();
            setInputDevice("");
            setOutputDevice("");
            startProcessing();
            }

    }//MainApp

	
	private ViewPager viewPager;
	private MyAdapter adapter;
	private Waterfall waterfall;
	private InText  inText;
	private OutText  outText;
	private MainApp _app;

    public MainActivity() {
        super();
		_app = new MainApp();
    }

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
		waterfall = (Waterfall) findViewById (R.id.waterfall);
		if (waterfall == null)
		    error("problem with the waterfall");
		viewPager = (ViewPager) findViewById (R.id.viewPager);
		PageListener pl = new PageListener();
		viewPager.setOnPageChangeListener(pl);
		Fragment modeFrags[] = getModeFragments();
		adapter = new MyAdapter(getSupportFragmentManager(), modeFrags);
		viewPager.setAdapter(adapter);
        viewPager.setCurrentItem(1);
		setMode(_app.pskMode());
	}
	
    //##################################################
    //# Messages
    //##################################################
    
    public void trace(String msg)
        {
        Log.i("bdigi", msg);
        }

    public void error(String msg)
        {
        Log.e("bdigi", msg);
        }

	public void error(String msg, Throwable e) 
	    {
		Log.e("bdigi", msg, e);
	    }
	    
    //##################################################
    //# Bind to App
    //##################################################
    
    public void setFrequency(double freq) {
        _app.setFrequency(freq);
    }
    
    public double getFrequency() {
        return _app.getFrequency();
    }
		
    public double getBandwidth() {
        return _app.bandwidth();
    }
    
    public Mode[] getModes() {
        return _app.modes();
    }
    
    public void setMode(Mode m) {
        setTitle("bdigi: " + m.name());
        _app.setMode(m);
    }
    
    //##################################################
    //# Button Callbacks
    //##################################################
    
    public void onRxTxClicked(View v) {
        ToggleButton btn = (ToggleButton) v;
        _app.setRxtx(btn.isChecked());
    }
		
    public void onClearClicked(View v) {
        if (inText != null)
            inText.clear();
        if (outText != null)
            outText.clear();
    }
		
    public void onAgcClicked(View v) {
        ToggleButton btn = (ToggleButton) v;
        _app.setAgc(btn.isChecked());
    }
		
    public void onAfcClicked(View v) {
        ToggleButton btn = (ToggleButton) v;
        _app.setAfc(btn.isChecked());
    }
		

    //##################################################
    //# Menu
    //##################################################

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.main, menu);
		return true;
	}
	
	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
	    switch (item.getItemId()) {
	        case R.id.action_home:
	            viewPager.setCurrentItem(1);
	            return true;
	        case R.id.action_config:
	        	viewPager.setCurrentItem(0);
	            return true;
	        case R.id.action_clear:
	        	if (outText != null)
	        	     outText.clear();
	        	if (inText != null)
	        	     inText.clear();
	            return true;
	        case R.id.action_close:
	            _app.stopProcessing(); //kill thread and close audio devices
	        	finish();
	            return true;
	        default:
	            return super.onOptionsItemSelected(item);
	    }
	}
	
	private String getInputText(int id) {
	    EditText text = (EditText) findViewById(id);
	    return text.getText().toString();
	}
	
	public void saveConfig(View view) {
	    String call    = getInputText(R.id.cfg_call);
	    String name    = getInputText(R.id.cfg_name);
	    String qth     = getInputText(R.id.cfg_qth);
	    String locator = getInputText(R.id.cfg_locator);
	    //trace("call:" + call);
	}
	
	public void setControls(Waterfall wf, OutText textView, InText editText) {
	    waterfall = wf;
	    outText = textView;
	    inText = editText;
	}
	
	private class PageListener extends SimpleOnPageChangeListener {
        public void onPageSelected(int position) {
            error("page: " + position);
        }
    }
    
    private Fragment[] getModeFragments() {
    
        final Context ctx = MainActivity.this;
        
        Mode modes[] = getModes();   
        Fragment frags[] = new Fragment[modes.length]; 
        for (int i=0 ; i < modes.length ; i++) {
            final Mode mode = modes[i];
            Fragment frag = new Fragment() {
            
                @Override
                public View onCreateView(LayoutInflater layoutInflater, ViewGroup viewGroup, Bundle bundle) {
                    LinearLayout layout = new LinearLayout(ctx);
                    layout.setOrientation(LinearLayout.VERTICAL);
                    TextView title = new TextView(ctx);
                    title.setClickable(true);
                    title.setOnClickListener(new OnClickListener() {
                        @Override
                        public void onClick(View v) {
                            setMode(mode);
                        }
                    });
                    title.setText("Mode: " + mode.name());
                    title.setTextAppearance(ctx, R.style.Title);
                    layout.addView(title);
        
                    Seq<Property<?>> props = mode.properties().properties();
          
                    for (int i=0 ; i < props.size() ; i++) {
                        Property p = props.apply(i);
        
                        if (p instanceof BooleanProperty) {
                            layout.addView(new PropWidget.BooleanPropertyWidget(ctx, (BooleanProperty)p));
                        } else if (p instanceof RadioProperty) {
                            layout.addView(new PropWidget.RadioPropertyWidget(ctx, (RadioProperty)p));
                        }
                    }
                    return layout;
                }
            };
            frags[i] = frag;
        } 
        return frags;    
    }


     
	
	class MyAdapter extends FragmentPagerAdapter
	{
	    private Fragment[] modeFragments;
		public MyAdapter(FragmentManager fm, Fragment[] modeFragments) {
			super(fm);
			this.modeFragments = modeFragments;
		}

		@Override
        public int getCount() {
            return 2+modeFragments.length;
        }

        @Override
        public Fragment getItem(int position) {
            switch(position) {
                case 0  : return ConfigFragment.newInstance();
                case 1  : return HomeFragment.newInstance();
                default : return modeFragments[position-2];
            }
        }
	}
	


}



