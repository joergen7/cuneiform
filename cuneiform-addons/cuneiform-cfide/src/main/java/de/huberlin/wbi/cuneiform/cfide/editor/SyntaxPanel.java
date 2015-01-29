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
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.KeyListener;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.event.CaretListener;
import javax.swing.text.Document;

public class SyntaxPanel extends JPanel {

	private static final long serialVersionUID = -385807381349136708L;

	private JTextPane editPane;
	protected StyleConf sc;
	
	public SyntaxPanel() {
		
		JPanel editPanel, innerPanel;
		JScrollPane scrollPane;
		
		
		setLayout( new BorderLayout() );
		sc = new DarkStyleConf();
		
		// edit pane
		editPane = new JTextPane();
		editPane.setBackground( sc.getBackgroundColor() );
		editPane.setFont( new Font( "Courier", Font.PLAIN, 14 ) );
		editPane.setForeground( Color.WHITE );
		editPane.setCaretColor( Color.WHITE );
		editPane.setCursor( new Cursor( Cursor.TEXT_CURSOR ) ); 
		editPane.setEditable( false );
		
		innerPanel = new JPanel();
		innerPanel.setLayout( new BorderLayout() );
		innerPanel.add( editPane );
		
		scrollPane = new JScrollPane( innerPanel );
		scrollPane.getVerticalScrollBar().setUnitIncrement( 32 );
		scrollPane.getHorizontalScrollBar().setUnitIncrement( 32 );
		
		editPanel = new JPanel();
		editPanel.setLayout( new BorderLayout() );
		editPanel.add( scrollPane, BorderLayout.CENTER );
		add( editPanel, BorderLayout.CENTER );


	}
	
	public int getCaretPosition() {
		return editPane.getCaretPosition();
	}
	
	public Document getDocument() {
		return editPane.getDocument();
	}
	
	public JTextPane getEditPane() {
		return editPane;
	}
	
	public String getText() {
		return editPane.getText();
	}

	public void setText( String text ) {
		editPane.setText( text );
		SyntaxListener.process( editPane, sc );
	}
	
	public void setEditable( boolean editable ) {
		editPane.setEditable( editable );
	}
	
	@Override
	public synchronized void addKeyListener( KeyListener listener ) {
		editPane.addKeyListener( listener );
	}
	
	public void addCaretListener( CaretListener listener ) {
		editPane.addCaretListener( listener );
	}
}
