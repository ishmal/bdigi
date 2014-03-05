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
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.SimpleOnPageChangeListener;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import org.bdigi.*;




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
    
    public void setInputDevice(String deviceName)
        {
        inputDevice_$eq(scala.Option.apply((AudioInputDevice)new org.bdigi.andy.AudioInput(this)));
        }
    
    public void setOutputDevice(String deviceName)
        {
        outputDevice_$eq(scala.Option.apply((AudioOutputDevice)new org.bdigi.andy.AudioOutput(this)));
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
        trace("updatespectrum");
        if (waterfall != null)
            waterfall.update(ps);
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
        super.start();
        }

}

	
	private ViewPager viewPager;
	private MyAdapter adapter;
	private static MainActivity _instance;
	private Waterfall waterfall;
	private MainApp _app;

	public static MainActivity getInstance() {
		return _instance;
	}

	public App getApp() {
		return _app;
	}
	
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

		
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		_instance = this;
		_app = new MainApp();
		waterfall = (Waterfall) findViewById (R.id.waterfall);
		setContentView(R.layout.activity_main);
		viewPager = (ViewPager) findViewById (R.id.viewPager);
		PageListener pl = new PageListener();
		viewPager.setOnPageChangeListener(pl);
		adapter = new MyAdapter(getSupportFragmentManager());
		viewPager.setAdapter(adapter);
	}
	
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
	            viewPager.setCurrentItem(0);
	            return true;
	        case R.id.action_config:
	        	viewPager.setCurrentItem(1);
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
	    trace("call:" + call);
	}
	
	private class PageListener extends SimpleOnPageChangeListener {
        public void onPageSelected(int position) {
        }
    }


    
    
	
	class MyAdapter extends FragmentPagerAdapter
	{
		public MyAdapter(FragmentManager fm) {
			super(fm);
		}

		@Override
        public int getCount() {
            return 2;
        }

        @Override
        public Fragment getItem(int position) {
            switch(position) {
                case 0 : return HomeFragment.newInstance();
                case 1 : return ConfigFragment.newInstance();
                default : return HomeFragment.newInstance();
            }
        }
	}
	


}



