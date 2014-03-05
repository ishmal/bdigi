package org.bdigi.andy;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.View;
import android.view.LayoutInflater;
import android.view.ViewGroup;


public class ConfigFragment extends Fragment
{
    public static ConfigFragment newInstance()
        {
        ConfigFragment f = new ConfigFragment();
        Bundle localBundle = new Bundle();
        f.setArguments(localBundle);
        return f;
        }

    @Override
    public View onCreateView(LayoutInflater layoutInflater, ViewGroup viewGroup, Bundle bundle)
        {
        return layoutInflater.inflate(R.layout.config, viewGroup, false);
        }
    
}
