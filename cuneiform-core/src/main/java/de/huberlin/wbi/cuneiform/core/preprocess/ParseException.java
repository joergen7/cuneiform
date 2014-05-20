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

package de.huberlin.wbi.cuneiform.core.preprocess;

public class ParseException extends RuntimeException {
	
	private static final long serialVersionUID = -3262992285049846787L;

	private Integer line;
	private Integer charPositionInLine;
	private String near;
	
	public ParseException( Integer line, Integer charPositionInLine, String near, String msg ) {
		super( msg );
		setLine( line );
		setCharPositionInLine( charPositionInLine );
		setNear( near );
	}
	
	public int getLine() {
		return line;
	}
	
	public int getCharPositionInLine() {
		return charPositionInLine;
	}
		
	public String getNear() {
		return near;
	}
	
	public boolean hasLine() {
		return line != null;
	}
	
	public boolean hasCharPositionInLine() {
		return charPositionInLine != null;
	}
	
	public boolean hasNear() {
		return near != null;
	}
	
	public void setLine( Integer line ) {
		
		if( line == null ) {
			this.line = null;
			return;
		}
		
		if( line < 1 )
			throw new RuntimeException( "Line must be a positive number. Received "+line+" instead." );
		this.line = line;
	}
	
	public void setNear( String near ) {
		
		if( near == null ) {
			this.near = null;
			return;
		}
		
		if( near.isEmpty() )
			throw new RuntimeException( "Near string must not be empty." );
		
		this.near = near;
	}
	
	public void setCharPositionInLine( Integer charPositionInLine ) {
		
		if( charPositionInLine == null ) {
			this.charPositionInLine = null;
			return;
		}
		
		if( charPositionInLine < 0 )
			throw new RuntimeException( "Character position in line must be a positive number. Received "+charPositionInLine+" instead." );
		
		this.charPositionInLine = charPositionInLine;
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		if( hasLine() )
			buf.append( "line " ).append( line ).append( ":" )
				.append( charPositionInLine ).append( ' ' );
		
		if( hasNear() )
			buf.append( "near " ).append( near ).append( ": " );
		
		buf.append( getMessage() );
		
		return buf.toString();
	}
}
