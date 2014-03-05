package org.bdigi.andy;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.View;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.LinearLayout;


public class HomeFragment extends Fragment
{
    public static HomeFragment newInstance()
        {
        HomeFragment f = new HomeFragment();
        Bundle localBundle = new Bundle();
        f.setArguments(localBundle);
        return f;
        }

    @Override
    public View onCreateView(LayoutInflater layoutInflater, ViewGroup viewGroup, Bundle bundle)
        {
        return layoutInflater.inflate(R.layout.home, viewGroup, false);
        }
}

