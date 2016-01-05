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



public class ApplyExpr extends BaseBlock implements SingleExpr {

	private int channel;
	private boolean rest;
	private CompoundExpr taskExpr;
	
	public ApplyExpr( int channel, boolean inheritsExtra ) {
		this( channel, inheritsExtra, null );
	}
	
	public ApplyExpr( int channel, boolean inheritsExtra, BaseBlock parent ) {

		super( parent );

		setChannel( channel );
		setRest( inheritsExtra );
	}
	
	public ApplyExpr( ApplyExpr template ) {
		
		if( template.taskExpr == null )
			throw new IllegalArgumentException( "Template apply expression has no task expression: "+template+"." );
		
		channel = template.channel;
		rest = template.rest;		
		taskExpr = new CompoundExpr( template.taskExpr );
		
		try {
			for( NameExpr ne : template.getFullNameSet() )
				putAssign( ne, new CompoundExpr( template.getExpr( ne ) ) );				
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e );
		}
	}
	
	public String getBlockString() {
		return super.toString();
	}
	
	public int getChannel() {
		return channel;
	}
	
	public Prototype getPrototype() throws NotDerivableException {
		
		for( SingleExpr se : taskExpr.getSingleExprList() )
			if( se instanceof LambdaExpr )
				return ( ( LambdaExpr )se ).getPrototype();
		
		throw new NotDerivableException( "Cannot derive prototype for "+taskExpr+"." );
	}
	
	public Block getParamBlock() throws NotDerivableException {
		
		Prototype prototype;
		Block paramBlock;
		
		prototype = getPrototype();
		
		paramBlock = new Block( getParent() );
		
		try {
			for( NameExpr nameExpr : getNameSet() ) {
				
				if( prototype.containsParam( nameExpr ) )
					paramBlock.putAssign( nameExpr, getExpr( nameExpr ) );
			}
		}
		catch( NotBoundException e ) {
			// cannot happen because we only ask for legal symbols
			throw new RuntimeException( e.getMessage() );
		}
		
		return paramBlock;		
	}
		
	public CompoundExpr getTaskExpr() {
		
		if( taskExpr == null )
			throw new NullPointerException( "Task expression never set." );
		
		return taskExpr;
	}
	
	@Override
	public StringExpr getStringExprValue( int i ) throws NotDerivableException {
		throw new NotDerivableException( "Cannot derive value in application." );
	}

	@Override
	public int hashCode() {
		
		String s;
		
		s = toString();
		s = s.substring( s.indexOf( ']' ) );
		
		return s.hashCode();
	}
	
	public boolean hasRest() {
		return rest;
	}
	
	public boolean hasTaskExpr() {
		return taskExpr != null;
	}
	
	public boolean isParamNormal() {
		
		CompoundExpr ce;
		
		try {
			for( NameExpr nameExpr : getNameSet() ) {
				
				ce = getExpr( nameExpr );
				if( !ce.isNormal() )
					return false;
			}

			return true;
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e.getMessage() );
		}
		
	}
	
	public void attemptPushRest() throws NotDerivableException {
		
		Block restBlock;
		Prototype prototype;
		NativeLambdaExpr nativeLambda;
		Block bodyBlock;
		
		try {
			prototype = getPrototype();
			
			restBlock = new Block();
			
			for( NameExpr name : getNameSet() )
				if( !prototype.containsParam( name ) )
					restBlock.putAssign( name, getExpr( name ) );
			
			if( !taskExpr.isNormal() )
				throw new NotDerivableException( "Task expression not in not normal." );
			
			for( SingleExpr se : taskExpr.getSingleExprList() ) {
				
				if( se instanceof NativeLambdaExpr ) {
				
				
					nativeLambda = ( NativeLambdaExpr )se;
									
					bodyBlock = nativeLambda.getBodyBlock();
					for( NameExpr name : bodyBlock.getNameSet() )
						bodyBlock.getExpr( name ).pushRest( restBlock );
					
					continue;
				}
				
				if( se instanceof ForeignLambdaExpr )
					continue;
				
				throw new RuntimeException(
					"Lambda expression expected." );

			}

		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e.getMessage() );
		}
			
	}
	
	@Override
	public void pushRest( BaseBlock restBlock ) {
		
		if( !rest )
			return;
		
		super.pushRest( restBlock );
		
		rest = false;
	}
	
	public void setChannel( int channel ) {
		
		if( channel < 1 ) 
			throw new SemanticModelException(
				String.valueOf( channel ),
				"Invalid channel "+channel+". Channel must be a positive integer." );
		
		this.channel = channel;
	}
		
	public void setRest( boolean inheritsExtra ) {
		this.rest = inheritsExtra;
	}
	
	public void setTaskExpr( CompoundExpr taskExpr ) {
		
		if( taskExpr == null )
			throw new NullPointerException( "Task expression must not be null." );
		
		this.taskExpr = taskExpr;
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		boolean comma;
		
		try {
		
			buf = new StringBuffer();
			
			buf.append( '[' ).append( channel ).append( "]apply(" );
			
			if( hasTaskExpr() )
				buf.append( " task: " ).append( taskExpr );
			
			comma = false;
			for( NameExpr name : getNameSet() ) {
				
				if( comma )
					buf.append( ',' );
				comma = true;
				
				buf.append( ' ' ).append( name.getId() ).append( ": " ).append( getExpr( name ) );
			}
			
			if( rest )
				buf.append( " ~" );
			
			buf.append( " )" );
			
			return buf.toString();
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e );
		}
	}

	@Override
	public int getNumAtom() throws NotDerivableException {
		
		Prototype prototype;
		NameExpr name;
		
		prototype = getPrototype();
		name = prototype.getOutput( channel-1 );
		
		if( name instanceof ReduceVar )
			throw new NotDerivableException( "Cannot derive cardinality of reduce output variable." );
		
		return 1;
	}
	
	@Override
	public boolean equals( Object obj ) {
		
		ApplyExpr other;
		String str1, str2;
		
		if( !( obj instanceof ApplyExpr ) )
			return false;
		
		other = ( ApplyExpr )obj;
		
		str1 = toString();
		str1 = str1.substring( str1.indexOf( ']' ) );
		
		str2 = other.toString();
		str2 = str2.substring( str2.indexOf( ']' ) );
		
		return str1.equals( str2 );
	}

	@Override
	public <T> T visit( NodeVisitor<? extends T> visitor ) throws HasFailedException, NotBoundException {
		return visitor.accept( this );
	}

}
