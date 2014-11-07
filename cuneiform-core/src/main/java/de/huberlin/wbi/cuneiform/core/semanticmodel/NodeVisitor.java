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


/**
 * @author jorgen
 *
 */
public interface NodeVisitor<T> {
	
	public T accept( StringExpr stringExpr );
	public T accept( QualifiedTicket qualifiedTicket );
	public T accept( NativeLambdaExpr nativeLambdaExpr );
	public T accept( NameExpr nameExpr ) throws HasFailedException;
	public T accept( LambdaType lambdaType );
	public T accept( DrawParam drawParam );
	public T accept( DataType dataType );
	public T accept( CondExpr condExpr ) throws HasFailedException;
	public T accept( Block block );
	public T accept( ApplyExpr applyExpr ) throws HasFailedException;
	public T accept( CompoundExpr compoundExpr ) throws HasFailedException;
	public T accept( CorrelParam correlParam );
	public T accept( ForeignLambdaExpr foreignLambdaExpr );
	public T accept( CurryExpr curryExpr ) throws HasFailedException;
	public T accept( Prototype prototype );
	public T accept( ReduceVar reduceVar );
	public T accept(TopLevelContext topLevelContext) throws HasFailedException;
	
}
