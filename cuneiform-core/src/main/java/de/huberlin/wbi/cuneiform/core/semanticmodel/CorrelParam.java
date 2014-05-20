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

package de.huberlin.wbi.cuneiform.core.semanticmodel;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class CorrelParam implements Param {

	private final Set<NameExpr> nameExprSet;
	
	public CorrelParam() {
		nameExprSet = new HashSet<>();
	}
	
	public void addName( NameExpr nameExpr ) {
		
		if( nameExpr == null )
			throw new NullPointerException( "Name expression must not be null." );
		
		nameExprSet.add( nameExpr );
	}
	
	public boolean contains( NameExpr nameExpr ) {
		return nameExprSet.contains( nameExpr );
	}
	
	@Override
	public int getNumParam() {
		return nameExprSet.size();
	}
	
	public NameExpr getLastNameExpr() {
		
		for( NameExpr nameExpr : nameExprSet )
			return nameExpr;
		
		throw new RuntimeException( "Name expression set must not be empty." );
	}
	
	@Override
	public Set<NameExpr> getNameExprSet() {

		// the parser already prevents that situation
		if( nameExprSet.isEmpty() )
			throw new RuntimeException( "Empty correlated parameter list." );
		
		if( nameExprSet.size() == 1 )
			throw new RuntimeException( "Correlated parameter list must have at least 2 entries." );
		
		return Collections.unmodifiableSet( nameExprSet );
	}
	
	public void remove( NameExpr nameExpr ) {
		if( !nameExprSet.remove( nameExpr ) )
			throw new RuntimeException( "Name expression '"+nameExpr+"' not member of correlated parameter." );
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( '[' );
		
		comma = false;
		for( NameExpr name : nameExprSet ) {
			
			if( comma )
				buf.append( ' ' );
			comma = true;
			
			buf.append( name );
		}
		
		buf.append( ']' );
		
		return buf.toString();
	}


	@Override
	public <T> T visit( NodeVisitor<? extends T> visitor ) {
		return visitor.accept( this );
	}
}
