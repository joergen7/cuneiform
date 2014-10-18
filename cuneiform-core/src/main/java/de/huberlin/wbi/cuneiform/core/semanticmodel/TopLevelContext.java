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

public class TopLevelContext extends BaseBlock {
	
	private List<CompoundExpr> targetList;

	public TopLevelContext() {
		this( null );
	}
	
	public TopLevelContext( BaseBlock parent ) {
		super( parent );
		targetList = new ArrayList<>();
	}
	
	public void addTarget( CompoundExpr target ) {
		targetList.add( target );
	}
	
	public void clearTargetList() {
		targetList.clear();
	}
	
	@Override
	public TopLevelContext clone() throws CloneNotSupportedException {
		
		TopLevelContext tlc;
		
		tlc = ( TopLevelContext )super.clone();
		targetList = new ArrayList<>();
		
		for( CompoundExpr ce : targetList )
			tlc.addTarget( ce );
		
		return tlc;
	}
	
	public String getBlockString() {
		return super.toString();
	}
	
	public List<CompoundExpr> getTargetList() {
		return Collections.unmodifiableList( targetList );
	}
	
	public CompoundExpr getTarget( int i ) {
		return targetList.get( i );
	}
	
	public int getTargetListSize() {
		return targetList.size();
	}
	
	public boolean isTargetListEmpty() {
		return targetList.isEmpty();
	}
	
	public boolean removeTarget( CompoundExpr target ) {		
		return targetList.remove( target );
	}
	
	public void setTarget( int i, CompoundExpr ce ) {
		targetList.set( i, ce );
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( super.toString() );
		
		for( CompoundExpr target : targetList )
			buf.append( target ).append( ";\n" );
		
		return buf.toString();
	}

	@Override
	public <T> T visit( NodeVisitor<? extends T> visitor ) throws HasFailedException, CloneNotSupportedException {
		return visitor.accept( this );
	}
}
