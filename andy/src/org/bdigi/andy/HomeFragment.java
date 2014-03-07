package org.bdigi.andy;

import android.app.Activity;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.View;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.EditText;
import android.widget.TextView;


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

    @Override
    public View onCreateView(LayoutInflater layoutInflater, ViewGroup viewGroup, Bundle bundle)
        {
        View v         = layoutInflater.inflate(R.layout.home, viewGroup, false);
        Waterfall wf   = (Waterfall) v.findViewById(R.id.waterfall);
        InText intxt   = (InText) v.findViewById(R.id.intext);
        OutText outtxt = (OutText) v.findViewById(R.id.outtext);
        getParent().setControls(wf, outtxt, intxt);
        return v;
        }
        
}

