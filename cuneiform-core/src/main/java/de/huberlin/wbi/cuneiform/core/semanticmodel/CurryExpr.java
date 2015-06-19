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


public class CurryExpr extends BaseBlock implements SingleExpr {

	private CompoundExpr taskExpr;
	
	@Override
	public int getNumAtom() throws NotDerivableException {
		return 1;
	}
	
	public CompoundExpr getTaskExpr() {
		
		if( taskExpr == null )
			throw new NullPointerException( "Task expression never set." );
		
		return taskExpr;
	}
	
	public boolean hasTaskExpr() {
		return taskExpr != null;
	}

	
	public void setTaskExpr( CompoundExpr taskExpr ) {
		
		if( taskExpr == null )
			throw new NullPointerException( "Task expression must not be null." );
		
		this.taskExpr = taskExpr;
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( "curry( task: " ).append( taskExpr );
		
		try {
			
			for( NameExpr nameExpr : getNameSet() )
				buf.append( ' ' ).append( nameExpr.getId() ).append( ": " ).append( getExpr( nameExpr ) );
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e.getMessage() );
		}
			
		
		buf.append( " )" );
		
		return buf.toString();
	}
	
	@Override
	public <T> T visit(NodeVisitor<? extends T> visitor) throws HasFailedException, NotBoundException {
		return visitor.accept( this );
	}

	@Override
	public StringExpr getStringExprValue(int i) throws NotDerivableException {
		throw new NotDerivableException( "Cannot derive value of curry expression." );
	}
}
