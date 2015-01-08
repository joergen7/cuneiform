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

package de.huberlin.wbi.cuneiform.logview.main;


import java.awt.BorderLayout;
import java.io.BufferedReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.swing.JFrame;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;
import de.huberlin.wbi.cuneiform.logview.graphview.GraphView;
import de.huberlin.wbi.cuneiform.logview.parallelismview.ParallelismView;
import de.huberlin.wbi.cuneiform.taskview.TaskBrowser;
import de.huberlin.wbi.cuneiform.taskview.TaskView;

public class Main {

	public static void main( String[] args ) throws Exception {


		JFrame frame;
		JSplitPane splitPane;
		TaskBrowser taskBrowser;
		TaskView taskView;
		Path logPath;
		String line;
		JTabbedPane tabbedPane;
		ParallelismView parallelismView;
		JsonReportEntry entry;
		GraphView graphView;
		Path buildPath;
		
		buildPath = Paths.get( System.getProperty( "user.home" ) ).resolve( ".cuneiform" );
		if( args.length > 0 )
			buildPath = Paths.get( args[ 0 ] );
		
		frame = new JFrame( "Cuneiform Log View" );
		
		frame.setSize( 1024, 768 );
		frame.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE );
		frame.setLayout( new BorderLayout() );
		
		
		taskView = new TaskView( buildPath );
		taskBrowser = new TaskBrowser( taskView );
		splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT, taskBrowser, taskView );
		splitPane.setDividerLocation( 200 );
		
		parallelismView = new ParallelismView();
		
		graphView = new GraphView();
		
		tabbedPane = new JTabbedPane();
		tabbedPane.addTab( "Task browser", splitPane );
		tabbedPane.addTab( "Parallelism", parallelismView );
		tabbedPane.addTab( "Invocation graph", graphView );
		
		frame.add( tabbedPane, BorderLayout.CENTER );
		
		logPath = Paths.get( "/tmp/cuneiform-stat.log" );
		try( BufferedReader reader = Files.newBufferedReader( logPath, Charset.forName( "UTF-8" ) ) ) {
			
			while( ( line = reader.readLine() ) != null ) {
				entry = new JsonReportEntry( line );
				taskBrowser.register( entry );
				parallelismView.register( entry );
				graphView.register( entry );
			}
			
		}
		
		taskBrowser.updateView();
		parallelismView.updateView();
		graphView.updateView();
		
		
		frame.setVisible( true );
		
    }
}
