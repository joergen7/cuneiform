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
