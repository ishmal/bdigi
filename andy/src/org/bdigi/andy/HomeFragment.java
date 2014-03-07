package org.bdigi.andy;

import android.app.Activity;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.View;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.EditText;
import android.widget.TabHost;
import android.widget.TextView;

import org.bdigi.*;
import org.bdigi.mode.Mode;
import scala.collection.Seq;

public class HomeFragment extends Fragment
{
    public static HomeFragment newInstance()
        {
        HomeFragment f = new HomeFragment();
        Bundle localBundle = new Bundle();
        f.setArguments(localBundle);
        return f;
        }
        
    private MainActivity getParent() {
        return (MainActivity) getActivity();
    }
    
    
    
    private void setUpTabs(final MainActivity par, final TabHost th) {

        th.setup();
        org.bdigi.mode.Mode[] modes = par.getModes();
        
        for (int i=0 ; i < modes.length ; i++) {
            final Mode mode = modes[i];
            String name = mode.name();
            TabHost.TabSpec ts = th.newTabSpec(name).setIndicator(name);
            ts.setContent(new TabHost.TabContentFactory() {
        
                public View createTabContent(String name) {
            
                    LinearLayout layout = new LinearLayout(par);
                    layout.setOrientation(LinearLayout.HORIZONTAL);
                
                    Seq<Property<?>> props = mode.properties().properties();
                
                    for (int i=0 ; i < props.size() ; i++) {
                        Property p = props.apply(i);
                
                        if (p instanceof BooleanProperty) {
                            layout.addView(new PropWidget.BooleanPropertyWidget(par, (BooleanProperty)p));
                        } else if (p instanceof RadioProperty) {
                            layout.addView(new PropWidget.RadioPropertyWidget(par, (RadioProperty)p));
                        }
                
                    }
                    return layout;
                } 
            });
            th.addTab(ts);
            
        }
        
        
        
    
    }

    @Override
    public View onCreateView(LayoutInflater layoutInflater, ViewGroup viewGroup, Bundle bundle)
        {
        MainActivity par = (MainActivity) getActivity();
        View v         = layoutInflater.inflate(R.layout.home, viewGroup, false);
        Waterfall wf   = (Waterfall) v.findViewById(R.id.waterfall);
        TabHost th     = (TabHost) v.findViewById(R.id.tabhost);
        setUpTabs(par, th);
        InText intxt   = (InText) v.findViewById(R.id.intext);
        OutText outtxt = (OutText) v.findViewById(R.id.outtext);
        par.setControls(wf, outtxt, intxt);
        return v;
        }
        
}

