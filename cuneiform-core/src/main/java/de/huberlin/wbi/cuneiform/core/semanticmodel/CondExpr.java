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


public class CondExpr implements SingleExpr {

	private final Prototype prototype;
	private final CompoundExpr ifExpr;
	private final Block thenBlock;
	private final Block elseBlock;
	private final int channel;
	
	public CondExpr( int channel, Prototype prototype, CompoundExpr ifExpr, Block thenBlock, Block elseBlock ) {

		if( prototype == null )
			throw new NullPointerException( "Prototype must not be null." );
		
		if( !prototype.getParamSet().isEmpty() )
			throw new SemanticModelException(
				prototype.toString(),
				"Prototype "+prototype+" is expected to not have any inputs." );
		
		if( ifExpr == null )
			throw new NullPointerException( "Condition expression must not be null." );
		
		if( thenBlock == null )
			throw new NullPointerException( "Then-block must not be null." );
		
		if( !thenBlock.hasParent() )
			throw new RuntimeException( "Then-block is expected to have a parent scope." );
		
		if( elseBlock == null )
			throw new NullPointerException( "Block must not be null." );
		
		if( !elseBlock.hasParent() )
			throw new RuntimeException( "Block is expected to have a parent scope." );
		
		if( channel < 1 )
			throw new SemanticModelException(
				String.valueOf( channel ),
				"Invalid channel "+channel+". Channel must be a positive integer." );
		
		this.channel = channel;
		this.elseBlock = elseBlock;
		this.thenBlock = thenBlock;
		this.ifExpr = ifExpr;
		this.prototype = prototype;

	}
	
	public int getChannel() {
		return channel;
	}
	
	public Block getElseBlock() {
		return elseBlock;
	}
	
	public CompoundExpr getIfExpr() {
		return ifExpr;
	}
	
	public NameExpr getOutputNameExpr() {
		return prototype.getOutput( channel-1 );
	}
	
	public Prototype getPrototype() {
		return prototype;
	}
	
	public Block getThenBlock() {
		return thenBlock;
	}
	
	@Override
	public String toString() {
		
		StringBuffer buf;
		
		buf = new StringBuffer();
		
		buf.append( '[' ).append( channel ).append( "]if" );
		buf.append( prototype ).append( '\n' );
		buf.append( ifExpr ).append( '\n' );
		buf.append( "then {\n" ).append( thenBlock );
		buf.append( "}\nelse {\n" ).append( elseBlock ).append( "}\n" );
		
		return buf.toString();
	}

	@Override
	public int getNumAtom() throws NotDerivableException {
		
		NameExpr name;
		
		name = prototype.getOutput( channel-1 );
		
		if( name instanceof ReduceVar )
			throw new NotDerivableException( "Cannot derive cardinality of reduce output variable." );
		
		return 1;
	}
	
	public CompoundExpr getThenExpr() {
		try {
			return thenBlock.getExpr( prototype.getOutput( channel-1 ) );
		}
		catch( NotBoundException e ) {
			throw new SemanticModelException( thenBlock.toString(), e.getMessage() );
		}
	}

	public CompoundExpr getElseExpr() {
		try {
			return elseBlock.getExpr( prototype.getOutput( channel-1 ) );
		}
		catch (NotBoundException e) {
			throw new RuntimeException( e.getMessage() );
		}
	}
	
	@Override
	public <T> T visit( NodeVisitor<? extends T> visitor ) throws HasFailedException, CloneNotSupportedException {
		return visitor.accept( this );
	}

	@Override
	public StringExpr getStringExprValue( int i ) throws NotDerivableException {
		throw new NotDerivableException( "Cannot derive value for conditional expression." );
	}
}
