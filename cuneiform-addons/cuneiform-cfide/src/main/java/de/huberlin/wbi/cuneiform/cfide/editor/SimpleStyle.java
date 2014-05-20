package de.huberlin.wbi.cuneiform.cfide.editor;

import java.awt.Color;

import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;


public class SimpleStyle {

	private Boolean it;
	private Boolean ul;
	private Boolean bf;
	private Color col;
	
	public SimpleStyle( Color col, Boolean it, Boolean bf, Boolean ul ) {
		
		setIt( it );
		setUl( ul );
		setBf( bf );
		setColor( col );
	}
	
	public void setIt( Boolean it ) {
		this.it = it;
	}
	
	public void setUl( Boolean ul ) {
		this.ul = ul;
	}
	
	public void setBf( Boolean bf ) {
		this.bf = bf;
	}
	
	public void setColor( Color col ) {
		this.col = col;
	}
	
	public void setColor( int r, int g, int b ) {
		col = new Color( r, g, b );
	}
	
	public void addStyleToDoc( StyledDocument doc, String key ) {
		
		Style style;
		
		style = doc.addStyle( key, null );
		
		if( ul != null )
			StyleConstants.setUnderline( style, ul );
		
		if( bf != null )
			StyleConstants.setBold( style, bf );
		
		if( it != null )
			StyleConstants.setItalic( style, it );
		
		if( col != null )
			StyleConstants.setForeground( style, col );
	}
	
	public static SimpleStyle createItStyle( Color c ) {		
		return new SimpleStyle( c, true, null, null ); 
	}

	public static SimpleStyle createItStyle() {		
		return new SimpleStyle( null, true, null, null ); 
	}
	
	public static SimpleStyle createBfStyle() {
		return new SimpleStyle( null, null, true, null );
	}
	
	public static SimpleStyle createUlStyle() {
		return new SimpleStyle( null, null, null, true );
	}
	
	public static SimpleStyle createPlainStyle( Color c ) {
		return new SimpleStyle( c, null, null, null );
	}
}
