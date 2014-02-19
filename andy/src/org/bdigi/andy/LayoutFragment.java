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


import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

/**
 * This is a simple Fragment that simply holds a layout.  This allows the
 * view pager to switch layouts
 */
public class LayoutFragment extends Fragment {

	//parent
	MainActivity par;
	//name of this view (for title)
	String name;
	//resource id
	int rid;
	
	
	public static LayoutFragment getInstance(MainActivity par, String name, int rid)
	{
		LayoutFragment af = new LayoutFragment();
		af.par = par;
		af.name = name;
		af.rid = rid;
		return af;
	}
		
	@Override
	public View onCreateView(LayoutInflater inflater,
	                           ViewGroup container,
	                           Bundle savedInstanceState) {
	    View result = inflater.inflate(rid, container, false);
	    return result;
	}


}
