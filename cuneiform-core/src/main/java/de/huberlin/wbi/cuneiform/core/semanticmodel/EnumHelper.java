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

import java.util.Set;

public class EnumHelper {

	private final Prototype prototype;
	private final BaseBlock bindingBlock;
	private final CompoundExpr taskExpr;
	private final Param[] paramArray;
	
	public EnumHelper( ApplyExpr applyExpr ) {
		this( applyExpr.getTaskExpr(), applyExpr );
	}
	
	public EnumHelper( CompoundExpr taskExpr, BaseBlock bindingBlock ) {
		
		Set<Param> paramSet;
		int i, n;
		SingleExpr se;
		
		if( bindingBlock == null )
			throw new IllegalArgumentException( "Binding block must not be null." );
		
		if( taskExpr == null )
			throw new IllegalArgumentException( "Task expression must not be null." );
		
		if( taskExpr.getNumSingleExpr() < 1 )
			throw new IllegalArgumentException( "Task expression should not be nil." );
		
		se = taskExpr.getSingleExpr( 0 );
		if( !( se instanceof LambdaExpr ) )
			throw new IllegalArgumentException( "Task expression must hold lambda expression." );
		
		this.taskExpr = taskExpr;		
		this.prototype = ( ( LambdaExpr )se ).getPrototype();
		this.bindingBlock = bindingBlock;
		
		// check if parameter bindings are consistent
		for( NameExpr paramName : prototype.getParamNameSet() ) {
			
			if( paramName.getId().equals( CfSemanticModelVisitor.LABEL_TASK ) )
				continue;
			
			if( !bindingBlock.containsName( paramName ) )
				throw new SemanticModelException(
					bindingBlock.toString(),
					"The parameter '"+paramName
					+"' is unbound in task application." );
		}

		paramSet = prototype.getParamSet();
		
		n = paramSet.size();
		
		// impose order on parameter set
		paramArray = new Param[ n ];
		i = 0;
		for( Param param : paramSet )
			paramArray[ i++ ] = param;
		
	}
	
	public int getCardinality() throws NotDerivableException {
		
		int i;
		int c;
		
		c = 1;
		for( i = 0; i < paramArray.length; i++ )
			c *= getParamCardinality( i );
		
		return c;
	}
	
	public LambdaExpr getSingularLambdaExpr( int idx ) throws NotDerivableException {
		
		int k;
		Param param;
		int c, i, j;
		NameExpr taskNameExpr;
		SingleExpr se;
		
		i = idx;
		taskNameExpr = new NameExpr( CfSemanticModelVisitor.LABEL_TASK );
		
		
		for( k = 0; k < paramArray.length; k++ ) {
			
			param = paramArray[ k ];
			
			if( param instanceof ReduceVar )
				continue;
			
			c = getParamCardinality( k );
			j = i%c;
			i /= c;
			
			if( !param.getNameExprSet().contains( taskNameExpr ) )
				continue;
			
			se = taskExpr.getSingleExpr( j );
			if( !( se instanceof LambdaExpr ) )
				throw new RuntimeException( "Expected lambda expression." );
			
			return ( LambdaExpr )se;
		}
		
		throw new RuntimeException( "Task parameter could not be addressed." );
	}

	
	public Block getSingularBindingBlock( int idx ) throws NotDerivableException {
		
		int i, j, k, c;
		Block block;
		Param param;
		CompoundExpr ce;
		
		block = new Block();
		i = idx;
		
		try {
			for( k = 0; k < paramArray.length; k++ ) {
				
				param = paramArray[ k ];
				
				if( param instanceof ReduceVar ) {
					
					block.putAssign(
						( ReduceVar )param,
						bindingBlock.getExpr( ( ReduceVar )param ) );
					
					continue;
				}
				
				c = getParamCardinality( k );
				j = i%c;
				i /= c;
				
				for( NameExpr name : param.getNameExprSet() ) {
					
					if( name.getId().equals( CfSemanticModelVisitor.LABEL_TASK ) )
						continue;
					
					ce = bindingBlock.getExpr( name );
					ce = new CompoundExpr( ce.getStringExprValue( j ) );
										
					if( ce.getNumAtom() != 1 )
						throw new RuntimeException( "Enumeration resulted in non-singular expression: "+ce );
					
					block.putAssign( name, ce );
				}
			}

		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e.getMessage() );
		}
		
		return block;
	}
	
	private int getParamCardinality( int i ) throws NotDerivableException {
		
		Param param;
		int n;
		CompoundExpr ce;
		
		
		try {
			
			param = paramArray[ i ];
			
			if( param instanceof ReduceVar )
				return 1;
			
			for( NameExpr name : param.getNameExprSet() ) {
				

				if( name.getId().equals( CfSemanticModelVisitor.LABEL_TASK ) )
					return taskExpr.getNumAtom();
				
				ce = bindingBlock.getExpr( name );
				
				n = ce.getNumAtom();
				
				return n;
			}
		}
		catch( NotBoundException e ) {
			
			System.err.println( "[task]" );
			System.err.println( taskExpr );
			System.err.println( "[binding]" );
			System.err.println( bindingBlock );
			System.err.println( "[end]" );
			throw new RuntimeException( e.getMessage() );
		}
		
		throw new RuntimeException( "Parameter must at least contain one parameter name." );
	}	
}
