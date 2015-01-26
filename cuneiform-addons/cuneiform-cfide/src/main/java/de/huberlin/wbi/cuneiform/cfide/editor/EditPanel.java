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
import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.Element;

public class EditPanel extends SyntaxPanel implements KeyListener, CaretListener {

	private static final long serialVersionUID = -7538677478512722934L;
	private static final String TITLE_UNSAVED = "Unsaved*";
	
	private JLabel posLabel;
	private ErrorTableModel errorTableModel;
	private File file;
	private String originalContent;
	private MainPanel mainPanel;
	
	public EditPanel( MainPanel mainPanel ) {
		this( mainPanel, null, "" );
	}
	
	public EditPanel( MainPanel mainPanel, File file, String content ) {
		
		JTable errorTable;
		JPanel posPanel;
		StaticPanel staticPanel;
		ExecutorService es;
		JScrollPane errorPane;
		JSplitPane editErrorSplitPane, editStaticPane;
		Component original;
		
		setMainPanel( mainPanel );
		setFile( file, content );
		setText( content );
		
		original = getComponent( 0 );
		remove( original );

		addKeyListener( this );
		addCaretListener( this );
		setEditable( true );
		
		// error table
		errorTableModel = new ErrorTableModel();
		errorTable = new JTable( errorTableModel );
		errorTable.getColumnModel().getColumn( 0 ).setMaxWidth( 60 );
		errorTable.getColumnModel().getColumn( 1 ).setMaxWidth( 60 );
		errorTable.getColumnModel().getColumn( 3 ).setMaxWidth( 60 );

		// caret position label
		posLabel = new JLabel( "1 : 1" );
	
		posPanel = new JPanel();
		posPanel.setLayout( new BoxLayout( posPanel, BoxLayout.X_AXIS ) );
		posPanel.add( Box.createHorizontalGlue() );
		posPanel.add( posLabel );

		add( posPanel, BorderLayout.SOUTH );
		
		staticPanel = new StaticPanel( this, errorTableModel );
		es = Executors.newSingleThreadExecutor();
		es.submit( staticPanel );
		es.shutdown();
		
		editStaticPane = new JSplitPane(
			JSplitPane.HORIZONTAL_SPLIT, staticPanel, original );
		editStaticPane.setDividerLocation( 450 );
		editStaticPane.setResizeWeight( 0 );
		editStaticPane.setOneTouchExpandable( true );
		
		errorPane = new JScrollPane( errorTable );
		errorPane.setBorder( BorderFactory.createTitledBorder( "Errors" ) );
		editErrorSplitPane = new JSplitPane(
			JSplitPane.VERTICAL_SPLIT,
			errorPane,
			editStaticPane );
		editErrorSplitPane.setDividerLocation( 100 );
		editErrorSplitPane.setResizeWeight( 0 );
		editErrorSplitPane.setOneTouchExpandable( true );
		
		add( editErrorSplitPane, BorderLayout.CENTER );
	}
	
	@Override
	public void caretUpdate( CaretEvent arg0 ) {
		
		int line, pos, col;
		Element root;
		
		if( posLabel == null )
			return;
		
		pos = getCaretPosition();
		root = getDocument().getDefaultRootElement();
		
		for( line = 0; line < root.getElementCount(); line++ )
			if( root.getElement( line ).getStartOffset() > pos )
				break;
		
		col = pos-root.getElement( line-1 ).getStartOffset()+1;
		
		posLabel.setText( line+" : "+col );
	}
	
	public ErrorTableModel getErrorTableModel() {
		return errorTableModel;
	}
	
	public File getFile() {
		return file;
	}
	
	public String getTitle() {
		
		String title;
		
		if( file == null )
			return TITLE_UNSAVED;
		
		title = file.getName();
		
		if( hasChanged() )
			title += "*";
		
		return title;
	}
	
	public boolean hasChanged() {		
		return !originalContent.equals( getText() );
	}
	
	public boolean hasFile() {
		return file != null;
	}
	
	@Override
	public void keyPressed( KeyEvent arg0 ) {}

	@Override
	public void keyReleased( KeyEvent arg0 ) {
		SyntaxListener.process( getEditPane(), sc );
		mainPanel.updateSelectedEditPanelTitle();
	}

	@Override
	public void keyTyped( KeyEvent arg0 ) {}
	
	public void revert() {
		setText( originalContent );
	}

	public void setFile( File file, String content ) {
		
		if( content == null )
			throw new NullPointerException( "Original content must not be null." );
		
		this.file = file;
		originalContent = content;
	}
	
	public void setMainPanel( MainPanel mainPanel ) {
		
		if( mainPanel == null )
			throw new NullPointerException( "Main panel must not be null." );
		
		this.mainPanel = mainPanel;
	}
}
