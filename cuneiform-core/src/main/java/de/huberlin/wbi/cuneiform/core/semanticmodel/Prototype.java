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
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class Prototype extends LambdaType implements Cloneable {

	private final List<NameExpr> outputList;
	private final Set<Param> paramSet;
	
	public Prototype() {
		outputList = new LinkedList<>();
		paramSet = new HashSet<>();
	}
	
	public void addOutput( NameExpr output ) {
		
		if( output == null )
			throw new NullPointerException( "Output must not be null." );
		
		outputList.add( output );
	}
	
	public void addOutput( List<NameExpr> ol ) {
		outputList.addAll( ol );
	}
	
	public void addParam( Param param ) {
		
		if( param == null )
			throw new NullPointerException( "Output must not be null." );
		
		paramSet.add( param );
	}
	
	public void addParam( Set<Param> ps ) {
		paramSet.addAll( ps );
	}
	
	@Override
	public Prototype clone() {
		
		Prototype child;
		
		child = new Prototype();
		
		child.addOutput( outputList );
		child.addParam( paramSet );
		
		return child;
	}
	
	public NameExpr getOutput( int i ) {
		return outputList.get( i );
	}
	
	public List<NameExpr> getOutputList() {
		
		if( outputList.isEmpty() )
			// parser already checks that
			throw new RuntimeException( "Empty output list." );
		
		return Collections.unmodifiableList( outputList );
	}
	
	public Set<Param> getParamSet() {
		return Collections.unmodifiableSet( paramSet );
	}
	
	public Set<NameExpr> getParamNameSet() {
		
		Set<NameExpr> nameSet;
		
		nameSet = new HashSet<>();
		
		for( Param param : paramSet )
			nameSet.addAll( param.getNameExprSet() );
		
		return nameSet;
	}
	
	public Set<NameExpr> getNonReduceParamNameSet() {
		
		Set<NameExpr> set;
		
		set = new HashSet<>();
		
		for( Param param : paramSet )
			if( !( param instanceof ReduceVar ) )
				set.addAll( param.getNameExprSet() );
		
		return set;
	}
	
	public boolean isTaskCorrelated() {
		
		for( Param param : paramSet )
			if( param instanceof CorrelParam )
				if( param.getNameExprSet().contains(
					CfSemanticModelVisitor.LABEL_TASK ) )
				
					return true;
		
		return false;
	}
	
	public boolean containsParam( String paramId ) {
		return containsParam( new NameExpr( paramId ) );
	}
	
	public boolean containsParam( NameExpr paramNameExpr ) {
		return getParamNameSet().contains( paramNameExpr );
	}
	
	public int getNumOutput() {
		return outputList.size();
	}
	
	public int getNumParam() {
		
		int n;
		
		n = 0;
		for( Param param : paramSet )
			n += param.getNumParam();
		
		return n;
	}
	
	public long getPrototypeId() {
		
		long h;
		
		h = 0;
		
		for( NameExpr output : outputList )
			h = HashHelper.add( h, output );
		
		for( NameExpr param : getParamNameSet() )
			h = HashHelper.add( h, param );
		
		return h;
	}
	
	public void removeParam( String name ) {
		removeParam( new NameExpr( name ) );
	}
	
	public void removeParam( NameExpr nameExpr ) {
		
		CorrelParam correlParam;
		int n;
		
		for( Param param : paramSet )
			if( param instanceof NameExpr ) {
				if( nameExpr.equals( param ) ) {
					paramSet.remove( nameExpr );
					return;
				}
			}
			else if( param instanceof CorrelParam ) {
			
				correlParam = ( CorrelParam )param;
				
				if( correlParam.contains( nameExpr ) ) {
					
					correlParam.remove( nameExpr );
					
					n = correlParam.getNumParam();
					
					if( n > 1 )
						return;
					
					if( n == 1 ) {
						
						paramSet.remove( correlParam );
						paramSet.add( correlParam.getLastNameExpr() );
						
						return;
					}
					
					throw new RuntimeException(
						"An empty correlated parameter set should never occur." );
				}
			}
		
		throw new RuntimeException( "Name expression '"+nameExpr+"' not member of parameter set." );
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		
		buf = new StringBuffer( "( " );
		
		for( NameExpr output : outputList )
			buf.append( output ).append( ' ' );
		
		buf.append( ": " );
		
		for( Param param : paramSet )
			buf.append( param ).append( ' ' );
		
		buf.append( ')' );
		
		return buf.toString();
	}
}
