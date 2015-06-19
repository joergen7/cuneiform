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

package de.huberlin.wbi.cuneiform.cfide.editor;

import java.awt.BorderLayout;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextPane;

import de.huberlin.wbi.cuneiform.core.preprocess.ChannelListener;
import de.huberlin.wbi.cuneiform.core.preprocess.ParseException;
import de.huberlin.wbi.cuneiform.core.preprocess.PreListener;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.staticreduction.DotNodeVisitor;
import de.huberlin.wbi.cuneiform.core.staticreduction.StaticNodeVisitor;
import de.huberlin.wbi.cuneiform.starlinger.StarlingerNodeVisitor;


public class StaticPanel extends JPanel implements Runnable {

	private static final long serialVersionUID = -5302001074704951699L;

	private SyntaxPanel channelPanel, reductionPanel, prePanel, modelPanel;

	private JLabel dagLabel;
	private JTextPane dotPane, starlingerPane;
	private SyntaxPanel srcPane;
	private ErrorTableModel errorTableModel;
	private int hash;
	
	public StaticPanel( SyntaxPanel srcPane, ErrorTableModel errorTableModel ) {
		
		JTabbedPane tabbedPane;
		JPanel panel;
		
		setSrcPane( srcPane );
		setErrorTableModel( errorTableModel );
		
		setLayout( new BorderLayout() );
		
		
		tabbedPane = new JTabbedPane();
		tabbedPane.setBorder( BorderFactory.createTitledBorder( "Static info" ) );
		add( tabbedPane, BorderLayout.CENTER );
		
		prePanel = new SyntaxPanel();
		tabbedPane.addTab( "(i) Pre", prePanel );
		
		channelPanel = new SyntaxPanel();
		tabbedPane.addTab( "(ii) Channel", channelPanel );
		
		modelPanel = new SyntaxPanel();
		tabbedPane.addTab( "(iii) Model", modelPanel );
		
		reductionPanel = new SyntaxPanel();
		tabbedPane.addTab( "(iv) Static red.", reductionPanel );
		
		starlingerPane = new JTextPane();
		tabbedPane.addTab( "Starlinger", new JScrollPane( starlingerPane ) );

		dotPane = new JTextPane();
		tabbedPane.addTab( "Dot", new JScrollPane( dotPane ) );
		
		dagLabel = new JLabel();

		panel = new JPanel();
		panel.setLayout( new BorderLayout() );
		panel.add( new JScrollPane( dagLabel ), BorderLayout.CENTER );
		
		
		tabbedPane.add( "DAG", panel );
		
		
	}
	
	public void setSrcPane( SyntaxPanel srcPane ) {
		
		if( srcPane == null )
			throw new NullPointerException( "Source pane must not be null." );
		
		this.srcPane = srcPane;
	}
	
	public void setErrorTableModel( ErrorTableModel errorTableModel ) {
		
		if( errorTableModel == null )
			throw new NullPointerException( "Error table model must not be null." );
		
		this.errorTableModel = errorTableModel;
	}
	
	@Override
	public void run() {

		String afterPre, afterChannel;
		TopLevelContext tlc;
		String dot;
		ImageIcon img;
		StaticNodeVisitor staticVisitor;
		DotNodeVisitor dotVisitor;
		CompoundExpr ce1;
		StarlingerNodeVisitor starlingerVisitor;
		String starlinger;
		String reduction;
		
		
		while( true ) {
			
			if( srcPane.getText().hashCode() != hash )
			
				try {
					
					hash = srcPane.getText().hashCode();
					
					errorTableModel.clear();
					
					afterPre = PreListener.process( srcPane.getText() );
					prePanel.setText( afterPre );
					
					afterChannel = ChannelListener.process( afterPre );
					channelPanel.setText( afterChannel );
					
					tlc = CfSemanticModelVisitor.process( afterChannel );
					modelPanel.setText( tlc.toString() );
					
					if( tlc.isTargetListEmpty() ) {
						starlinger = "{ 'nodes'=>{},'edges'=>{}}";
						dot = "digraph G {}";
						reduction = "";
					}
					else {
						
						staticVisitor = new StaticNodeVisitor( tlc );
						
						ce1 = tlc.visit( staticVisitor );
						reduction = ce1.toString()+";";
						
						starlingerVisitor = new StarlingerNodeVisitor();
						
						ce1.visit( starlingerVisitor );
						starlinger = starlingerVisitor.toString();
						
						dotVisitor = new DotNodeVisitor();
						
						ce1.visit( dotVisitor );
						dot = dotVisitor.toString();
						
					}
					
					reductionPanel.setText( reduction );
					starlingerPane.setText( starlinger );
					dotPane.setText( dot );
					
					img = runGraphViz( dot );
					dagLabel.setIcon( img );
					
				}
				catch( ParseException e ) {
					errorTableModel.add( e );
				}
				catch( RuntimeException e ) {
					e.printStackTrace();
				}
				catch( HasFailedException e ) {
					e.printStackTrace();
				} catch (NotBoundException e) {
					e.printStackTrace();
				}
				finally {
					errorTableModel.fireTableDataChanged();				
				}
				
			try {
				Thread.sleep( 2000 );
			}
			catch( InterruptedException e ) {
				e.printStackTrace();
			}
		}
	

	}
	
	private static ImageIcon runGraphViz( String dot ) {
		
		ImageIcon img;
		
		
		
		ProcessBuilder builder;
		Process p;
		ExecutorService executorService;
		BufferedImage rawImage;	
		
		builder = new ProcessBuilder( "/bin/bash", "-c", "dot -T png" );

		try {
		p = builder.start();

		executorService = Executors.newSingleThreadExecutor();
		executorService.submit( new ProducerThread( p, dot ) );
		executorService.shutdown();
		
		try( InputStream inStream =  p.getInputStream() ) {
			
			
			rawImage = ImageIO.read( inStream );
			if( rawImage == null ) {
				System.err.println( "Cannot produce image. Is graphviz installed properly?" );
				return null;
			}
			
			rawImage.flush();

			img = new ImageIcon( rawImage );

			
			return img;
		}
		}
		catch( IOException e ) {
			e.printStackTrace();
		}
		
		return null;
		
		
	}
	
	private static class ProducerThread implements Runnable {

		private Process p;
		private String dot;
		
		public ProducerThread( Process p, String dot ) {
			
			if( p == null )
				throw new NullPointerException( "Process must not be null." );
			
			if( dot == null )
				throw new NullPointerException( "Dot string must not be null." );
			
			this.p = p;
			this.dot = dot;
		}
		
		@Override
		public void run() {

			
			
			
			try( OutputStream outStream = p.getOutputStream() ) {
				
				outStream.write( dot.getBytes() );
				outStream.close();
			} catch( IOException e ) {
				e.printStackTrace();
			}
			
		}
		
		
	}
	
}
