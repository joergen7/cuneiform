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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CompoundExpr implements CfNode, Cloneable {

	private final List<SingleExpr> singleExprList;
	
	public CompoundExpr() {
		singleExprList = new ArrayList<>();
	}
	
	public CompoundExpr( SingleExpr se ) {
		this();
		addSingleExpr( se );
	}
	
	public void addCompoundExpr( CompoundExpr ce ) {
		
		if( ce == null )
			throw new NullPointerException( "Compound expression must not be null." );
		
		singleExprList.addAll( ce.getSingleExprList() );
	}
	
	public void addSingleExpr( SingleExpr singleExpr ) {
		
		if( singleExpr == null )
			throw new NullPointerException( "Single expression must not be null." );
		
		singleExprList.add( singleExpr );
	}
	
	@Override
	public CompoundExpr clone() {
		
		CompoundExpr ce;
		
		ce = new CompoundExpr();
		ce.addCompoundExpr( this );
		
		return ce;
	}
	
	public int getNumAtom() throws NotDerivableException {
		
		int n;
		
		n = 0;
		
		for( SingleExpr se : singleExprList )
			n += se.getNumAtom();
		
		return n;
	}
	
	public int getNumSingleExpr() {
		return singleExprList.size();
	}
	
	public SingleExpr getSingleExpr( int i ) {
		return singleExprList.get( i );
	}
	
	public List<SingleExpr> getSingleExprList() {
		return Collections.unmodifiableList( singleExprList );
	}
	
	public StringExpr getStringExprValue( int idx ) throws NotDerivableException {
		
		int i, j, k;
		
		if( idx < 0 )
			throw new IndexOutOfBoundsException(
				"Queried entry "+idx+". Negative indexes are not allowed." );
		
		if( idx >= getNumAtom() )
			throw new IndexOutOfBoundsException(
				"Queried entry "+idx+" but the compound expression has only "
				+getNumAtom()+" elements." );
		
		j = 0;
		i = idx;
		
		while( true ) {
			
			k = i-getSingleExpr( j ).getNumAtom();
			
			if( k < 0 )
				return getSingleExpr( j ).getStringExprValue( i );
			
			j++;
			i = k;
		}
	}
	
	public boolean isNormal() {
		
		QualifiedTicket qt;
		
		for( SingleExpr se : singleExprList ) {
			
			if( se instanceof StringExpr )
				continue;
			
			if( se instanceof LambdaExpr )
				continue;
			
			if( !( se instanceof QualifiedTicket ) )
				return false;
			
			qt = ( QualifiedTicket )se;
			
			try {
				if( !qt.getOutputValue().isNormal() )
					return false;
			} catch( NotDerivableException e ) {
				return false;
			}
		}
		
		return true;
	}
	
	public List<String> normalize() throws NotDerivableException {
		
		List<String> normalList;
		QualifiedTicket qt;
		
		normalList = new ArrayList<>();
		
		for( SingleExpr se : singleExprList ) {
			
			if( se instanceof StringExpr ) {
				
				normalList.add( ( ( StringExpr )se ).getContent() );
				continue;
			}
			
			if( se instanceof QualifiedTicket ) {
				
				qt = ( QualifiedTicket )se;			
				normalList.addAll( qt.getOutputValue().normalize() );
				continue;
			}
			
			throw new NotDerivableException( "Non-normalizable expression encountered." );			
		}
		
		return normalList;
	}
	
	public void pushRest( Block restBlock ) {
		
		ApplyExpr applyExpr;
		
		for( SingleExpr se : singleExprList )
			
			if( se instanceof ApplyExpr ) {

				applyExpr = ( ApplyExpr )se;
				applyExpr.pushRest( restBlock );
			}
	}
	
	public void remove( SingleExpr se ) {
		if( !singleExprList.remove( se ) )
			throw new RuntimeException( "Single expression is no member of this compound expression." );
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		boolean comma;
		
		try {
			
			if( this.getNumAtom() == 0 )
				return "nil";
			
			buf = new StringBuffer();
			
			comma = false;
			for( SingleExpr singleExpr : singleExprList ) {
				
				if( singleExpr.getNumAtom() == 0 )
					continue;
				
				if( comma )
					buf.append( ' ' );
				
				comma = true;
				
				buf.append( singleExpr );
			}
			
			return buf.toString();
		}
		catch( NotDerivableException e ) {
		
			if( singleExprList.isEmpty() )
				return "nil";
			
			buf = new StringBuffer();
			
			comma = false;
			for( SingleExpr singleExpr : singleExprList ) {
				
				if( comma )
					buf.append( ' ' );
				
				comma = true;
				
				buf.append( singleExpr );
			}
			
			return buf.toString();
		}
	}
	
	@Override
	public <T> T visit(NodeVisitor<? extends T> visitor) throws HasFailedException {
		return visitor.accept(this);
	}
}
