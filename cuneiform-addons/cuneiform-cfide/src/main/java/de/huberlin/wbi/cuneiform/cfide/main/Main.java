/*******************************************************************************
 * In the Hi-WAY project we propose a novel approach of executing scientific
 * workflows processing Big Data, as found in NGS applications, on distributed
 * computational infrastructures. The Hi-WAY software stack comprises the func-
 * tional workflow language Cuneiform as well as the Hi-WAY ApplicationMaster
 * for Apache Hadoop 2.x (YARN).
 *
 * List of Contributors:
 *
 * Jörgen Brandt (HU Berlin)
 * Marc Bux (HU Berlin)
 * Ulf Leser (HU Berlin)
 *
 * Jörgen Brandt is funded by the European Commission through the BiobankCloud
 * project. Marc Bux is funded by the Deutsche Forschungsgemeinschaft through
 * research training group SOAMED (GRK 1651).
 *
 * Copyright 2014 Humboldt-Universität zu Berlin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.huberlin.wbi.cuneiform.cfide.main;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import de.huberlin.wbi.cuneiform.cfide.editor.MainPanel;

public class Main implements Runnable {

	public static void main( String[] args )  {
		
		Main m;
		
		m = new Main();
		
		SwingUtilities.invokeLater( m );
	}

	@Override
	public void run() {
		
		JFrame frame;
		JMenuBar menuBar;
		MainPanel editRunPanel;
				
		frame = new JFrame( "Cuneiform Editor" );
		frame.setSize( 1000, 750 );
		frame.setLocation( 0, 50 );
		frame.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE );
		
		editRunPanel = new MainPanel( frame );
		frame.addWindowListener( editRunPanel );
		
		menuBar = editRunPanel.getMenuBar();
		
		frame.setJMenuBar( menuBar );
		frame.add( editRunPanel );
		frame.setVisible( true );
	}
}
