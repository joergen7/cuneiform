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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public abstract class BaseBlock implements CfNode, Cloneable {

	private Map<NameExpr,CompoundExpr> assignMap;
	private BaseBlock parent;
	
	public BaseBlock() {
		this( null );
	}
	
	public BaseBlock( BaseBlock parent ) {
		assignMap = new HashMap<>();
		setParent( parent );
	}
	
	public void clear() {
		assignMap.clear();
	}
	
	@Override
	public BaseBlock clone() throws CloneNotSupportedException {
		
		BaseBlock block1;
		
		block1 = ( BaseBlock )super.clone();
		block1.setParent( parent );

		try {
			
			for( NameExpr key : getNameSet() )
				block1.putAssign( key, getExpr( key ) );
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e );
		}
		
		return block1;
		
	}
	
	public boolean containsName( NameExpr nameExpr ) {
		return assignMap.containsKey( nameExpr );
	}
	
	public CompoundExpr getExpr( NameExpr name ) throws NotBoundException {
		
		CompoundExpr ce;
		
		ce = assignMap.get( name );
		
		if( ce != null )
			return ce;
			
		if( parent == null )
			throw new NotBoundException( "A name '"+name+"' is not bound in this block." );
		
		return parent.getExpr( name );
	}
	
	public CompoundExpr getExpr( String name ) throws NotBoundException {
		return getExpr( new NameExpr( name ) );
	}
	
	public Set<NameExpr> getNameSet() {
		return assignMap.keySet();
	}	
	
	public Map<NameExpr,CompoundExpr> getParamBindMap() {
		return Collections.unmodifiableMap( assignMap );
	}
	
	public BaseBlock getParent() {
		return parent;
	}
	
	public boolean hasParent() {
		return parent != null;
	}
	
	public boolean isEmpty() {
		return assignMap.isEmpty();
	}
	
	/**
	 * @throws NotDerivableException  
	 */
	public void pushRest( BaseBlock restBlock ) {
		
		try {
			
			for( NameExpr name : restBlock.getNameSet() )
				if( !containsName( name ) )
					putAssign( name, restBlock.getExpr( name ) );
			
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e.getMessage() );
		}
	}
	
	public void putAssign( NameExpr key, CompoundExpr value ) {
		
		if( key == null )
			throw new NullPointerException( "Name expression must not be null." );
		
		if( value == null )
			throw new NullPointerException( "Compound expression must not be null." );
		
		assignMap.put( key, value );
	}
	
	public void removeAssign( String name ) {
		removeAssign( new NameExpr( name ) );
	}
	
	public void removeAssign( NameExpr nameExpr ) {
		
		if( assignMap.remove( nameExpr ) == null )
			throw new NullPointerException( "Cannot remove. Assignment does not exist." );
	}
	
	public void setParamBindMap( Map<NameExpr,CompoundExpr> paramBindMap ) {
		
		if( assignMap == null )
			throw new NullPointerException( "Parameter binding map must not be null." );
		
		assignMap = paramBindMap;
	}
	
	public void setParent( BaseBlock parent ) {
		this.parent = parent;
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		List<NameExpr> nameExprSet;
		
		buf = new StringBuffer();
		
		nameExprSet = new LinkedList<>();
		
		nameExprSet.addAll( assignMap.keySet() );
		Collections.sort( nameExprSet );
		
		for( NameExpr nameExpr : nameExprSet )
			buf.append( nameExpr ).append( " = " ).append( assignMap.get( nameExpr ) ).append( ";\n" );
		
		return buf.toString();
	}
}
