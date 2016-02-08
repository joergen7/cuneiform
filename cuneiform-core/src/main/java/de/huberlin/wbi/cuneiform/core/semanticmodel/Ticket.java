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
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.json.JSONException;
import org.json.JSONObject;


public class Ticket extends Block {

	private final ForeignLambdaExpr lambdaExpr;
	private final Block bodyBlock;
	private final UUID runId;
	
	public Ticket( ForeignLambdaExpr lambdaExpr, BaseBlock bindingBlock, UUID runId ) {
		
		if( bindingBlock == null )
			throw new NullPointerException( "Binding block must not be null." );

		if( runId == null )
			throw new NullPointerException( "Run ID must not be null." );
		
		if( lambdaExpr == null )
			throw new NullPointerException(
				"Foreign lambda expression must not be null." );
		
		setParamBindMap( bindingBlock.getParamBindMap() );

		this.lambdaExpr = lambdaExpr;		
		this.runId = runId;

		bodyBlock = new Block();
		
		if( !isReady() )
			throw new RuntimeException( "Attempted to create non-ready ticket." );
	}

	
	public int getNumAtom( int channel ) throws NotDerivableException {
		
		NameExpr outputVar;
		
		try {
			
			outputVar = getPrototype().getOutput( channel-1 );
			
			if( outputVar instanceof ReduceVar )
				return bodyBlock.getExpr( getPrototype().getOutput( channel-1 ) ).getNumAtom();
	
			return 1;
		}
		catch( NotBoundException e ) {
			throw new NotDerivableException( e.getMessage() );
		}
	}
	
	public String getBody() {
		return lambdaExpr.getBody();
	}
	
	public JsonReportEntry getExecutableLogEntry() {
		
		JsonReportEntry entry;
		JSONObject obj;
		
		
		try {
			
			obj = new JSONObject();
			obj.put( "lambda", lambdaExpr );
			obj.put( "bind", getParamBindMap() );
			
			entry = new JsonReportEntry(
					runId, lambdaExpr.getLambdaId(), lambdaExpr.getTaskName(),
					lambdaExpr.getLangLabel(), getTicketId(), null,
					JsonReportEntry.KEY_INVOC_EXEC, obj );
			
			return entry;
		
		}
		catch( JSONException e ) {
			throw new RuntimeException( e );
		}
	}
	
	public long getLambdaId() {
		return lambdaExpr.getLambdaId();
	}
	
	public String getLangLabel() {
		return lambdaExpr.getLangLabel();
	}
	
	public List<NameExpr> getOutputList() {
		return lambdaExpr.getPrototype().getOutputList();
	}
	
	public CompoundExpr getOutputValue( int channel ) throws NotBoundException {
		return bodyBlock.getExpr( lambdaExpr.getPrototype().getOutput( channel-1 ) );
	}
	
	public CompoundExpr getOutputValue( NameExpr name ) throws NotBoundException {
		return bodyBlock.getExpr( name );
	}
	
	public Set<Param> getParamSet() {
		return lambdaExpr.getPrototype().getParamSet();
	}
	
	public Set<NameExpr> getParamNameSet() {
		return lambdaExpr.getPrototype().getParamNameSet();
	}
	
	public Prototype getPrototype() {
		return lambdaExpr.getPrototype();
	}
	
	public UUID getRunId() {
		return runId;
	}
	
	public String getTaskName() {
		return lambdaExpr.getTaskName();
	}
	
	public long getTicketId() {
		
		long h;
		
		h = toString().hashCode();
		if( h < 0 )
			h = -h;
		
		return h;
		
	}

	public boolean isNormal() {
		
		try {
			
			for( NameExpr name : getNameSet() )
				if( !getExpr( name ).isNormal() )
					return false;
			
			return true;
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e );
		}
	}
	
	public boolean isReady() {
		
		try {
			for( NameExpr name : getNameSet() )
				if( !getExpr( name ).isNormal() )
					return false;
			
			return true;
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e );
		}
	}
	
	public boolean hasTaskName() {
		return lambdaExpr.hasTaskName();
	}
	
	public boolean isEvaluated() {
		return !bodyBlock.isEmpty();
	}
	
	public void setValue( NameExpr outputNameExpr, CompoundExpr value ) {
		bodyBlock.putAssign( outputNameExpr, value );
	}
	
	@Override
	public String toString() {

		StringBuffer buf;
		List<String> list;
		
		try {
		
			buf = new StringBuffer();
						
			buf.append( "ticket(" );
			
			buf.append( " task: " ).append( lambdaExpr );
			
			list = new LinkedList<>();
			for( NameExpr name : getNameSet() )
				list.add( name.getId() );
			
			Collections.sort( list );
			
			for( String name : list )
				buf.append( ' ' ).append( name ).append( ": " ).append( getExpr( name ) );
			
			buf.append( " )" );
						
			return buf.toString();
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e.getMessage() );
		}
	}

}
