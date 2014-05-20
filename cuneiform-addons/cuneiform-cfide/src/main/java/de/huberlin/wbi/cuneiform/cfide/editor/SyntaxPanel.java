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
	
	public SyntaxPanel() {
		
		JPanel editPanel, innerPanel;
		JScrollPane scrollPane;
		
		
		setLayout( new BorderLayout() );
		
		// edit pane
		editPane = new JTextPane();
		editPane.setBackground( StyleConf.COL_BACKGROUND );
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
		SyntaxListener.process( editPane, StyleConf.createDefaultStyleConf() );
	}
	
	public void setEditable( boolean editable ) {
		editPane.setEditable( editable );
	}
	
	public void addKeyListener( KeyListener listener ) {
		editPane.addKeyListener( listener );
	}
	
	public void addCaretListener( CaretListener listener ) {
		editPane.addCaretListener( listener );
	}
}
