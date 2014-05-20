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

public class StringExpr implements SingleExpr {

	private String value;
	
	public StringExpr( String content ) {
		setContent( content );
	}
	
	@Override
	public boolean equals( Object obj ) {
		
		StringExpr other;
		
		if( !( obj instanceof StringExpr ) )
			return false;
		
		other = ( StringExpr )obj;
		
		return other.getContent().equals( value );
			
	}
	
	public String getContent() {
		return value;
	}
	
	@Override
	public int getNumAtom() throws NotDerivableException {		
		return 1;
	}
	
	public void setContent( String content ) {
		
		if( content == null )
			throw new NullPointerException( "Content string must not be null." );
		
		this.value = content;
	}
	
	@Override
	public String toString() {
		return "'"+value+"'";
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}
	
	/* (non-Javadoc)
	 * @see de.huberlin.cuneiform.core.semanticmodel.Node#visit(de.huberlin.cuneiform.core.semanticmodel.NodeVisitor)
	 */
	@Override
	public <T> T visit( NodeVisitor<? extends T> visitor ) {
		return visitor.accept( this );
	}
}
