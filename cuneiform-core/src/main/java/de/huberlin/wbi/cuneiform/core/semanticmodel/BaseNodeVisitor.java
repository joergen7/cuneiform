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

public abstract class BaseNodeVisitor implements NodeVisitor<CompoundExpr> {

	@Override
	public CompoundExpr accept( Block block ) {
		throw new RuntimeException( "Block is not an expression." );
	}
	
	@Override
	public CompoundExpr accept( ForeignLambdaExpr foreignLambdaExpr ) {
		return new CompoundExpr( foreignLambdaExpr );
	}
		
	@Override
	public CompoundExpr accept( NativeLambdaExpr nativeLambdaExpr ) {
		return new CompoundExpr( nativeLambdaExpr );
	}
	
	@Override
	public CompoundExpr accept( StringExpr stringExpr ) {
		return new CompoundExpr( stringExpr );
	}
	
	@Override
	public CompoundExpr accept( QualifiedTicket qualifiedTicket ) {
		return new CompoundExpr( qualifiedTicket );
	}
	
	@Override
	public CompoundExpr accept( LambdaType lambdaType ) {
		throw new RuntimeException( "Lambda type is not an expression." );
	}

	@Override
	public CompoundExpr accept( DrawParam drawParam ) {
		throw new RuntimeException( "Draw parameter is not an expression." );
	}

	@Override
	public CompoundExpr accept( DataType dataType ) {
		throw new RuntimeException( "Data type is not an expression." );
	}
	
	@Override
	public CompoundExpr accept( CorrelParam correlParam ) {
		throw new RuntimeException( "Correlated parameter is not an expression." );
	}

	@Override
	public CompoundExpr accept( Prototype prototype ) {
		throw new RuntimeException( "Prototype is not an expression." );
	}

	@Override
	public CompoundExpr accept( ReduceVar reduceVar ) {
		throw new RuntimeException( "Reduce variable is not an expression." );
	}



}
