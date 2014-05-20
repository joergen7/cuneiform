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

import java.util.HashSet;
import java.util.Set;


public class NameExpr implements SingleExpr, Param, Comparable<NameExpr> {
	
	private String id;
	private Type type;
	
	public NameExpr( String id ) {
		this( id, null );
	}
	
	public NameExpr( String id, Type type ) {
		setId( id );
		setType( type );
	}
	
	@Override
	public boolean equals( Object obj ) {
		
		NameExpr other;
		
		if( !( obj instanceof NameExpr ) )
			return false;
		
		other = ( NameExpr )obj;
		return id.equals( other.getId() );
	}
	
	public String getId() {
		return id;
	}
	
	@Override
	public int getNumParam() {
		return 1;
	}
	
	public Type getType() {
		return type;
	}
	
	@Override
	public int hashCode() {
		return id.hashCode();
	}

	public boolean hasType() {
		return type != null;
	}
	
	public void setId( String id ) {
		
		if( id == null )
			throw new NullPointerException( "Id string must not be null." );
		
		if( id.isEmpty() )
			throw new RuntimeException( "Id string must not be empty." );
		
		this.id = id;
	}
	
	public void setType( Type type ) {		
		this.type = type;
	}

	public ReduceVar toReduceVar() {
		return new ReduceVar( id, type );
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( id );
		
		if( type != null )
			buf.append( type );
		
		return buf.toString();
	}

	@Override
	public Set<NameExpr> getNameExprSet() {

		Set<NameExpr> nameSet;
		
		nameSet = new HashSet<>();
		nameSet.add( this );
		
		return nameSet;
	}

	@Override
	public int getNumAtom() throws NotDerivableException {
		throw new NotDerivableException( "Name expression cannot hold size information." );
	}

	@Override
	public int compareTo( NameExpr arg0 ) {
		return arg0.getId().compareTo( id );
	}
	@Override
	public <T> T visit( NodeVisitor<? extends T> visitor ) {
		return visitor.accept( this );
	}
}
