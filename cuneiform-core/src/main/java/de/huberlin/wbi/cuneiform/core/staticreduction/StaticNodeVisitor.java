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


package de.huberlin.wbi.cuneiform.core.staticreduction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import de.huberlin.wbi.cuneiform.core.preprocess.ChannelListener;
import de.huberlin.wbi.cuneiform.core.preprocess.PreListener;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.BaseBlock;
import de.huberlin.wbi.cuneiform.core.semanticmodel.BaseNodeVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Block;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CondExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CurryExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.LambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NativeLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Prototype;
import de.huberlin.wbi.cuneiform.core.semanticmodel.SemanticModelException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.SingleExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;

/**
 * @author jorgen
 *
 */
public class StaticNodeVisitor extends BaseNodeVisitor {

	private final LinkedList<BaseBlock> blockStack;
	private BaseBlock currentBlock;
	private final Set<NameExpr> freeVarSet;
	
	public StaticNodeVisitor( BaseBlock currentBlock ) {
		blockStack = new LinkedList<>();
		this.currentBlock = currentBlock;
		freeVarSet = new HashSet<>();
	}

	@Override
	public CompoundExpr accept( ApplyExpr applyExpr ) throws HasFailedException {
		SingleExpr se;
		NativeLambdaExpr lambda;
		NameExpr targetNameExpr;
		CompoundExpr targetCompoundExpr;
		int channel;
		CompoundExpr taskResult, taskOriginal;
		ApplyExpr applyExpr1;
		boolean rest;
		
		
		// try to reduce the task expression
		taskOriginal = applyExpr.getTaskExpr();
		taskResult = taskOriginal.visit( this );
		
		channel = applyExpr.getChannel();
		rest = applyExpr.hasRest();
		
		applyExpr1 = new ApplyExpr( channel, rest );
		applyExpr1.setTaskExpr( taskResult );
		
		// try to reduce the parameter list
		try {
			for( NameExpr nameExpr : applyExpr.getNameSet() ) {
				CompoundExpr expr = applyExpr.getExpr( nameExpr );
				applyExpr1.putAssign( nameExpr, expr.visit( this ));
			}
		}
		catch( NotBoundException e ) {
			throw new RuntimeException( e.getMessage() );
		}
		
		// continue only if one single lambda expression is applied
		if( taskResult.getNumSingleExpr() != 1 )
			return new CompoundExpr( applyExpr1 );
			
		// fetch that lambda expression
		se = taskResult.getSingleExpr( 0 );
		
		if( se == null )
			throw new NullPointerException( "Expression must not be null." );
		
		// continue only if that single lambda expression is native
		if( se instanceof NativeLambdaExpr ) {
		
			lambda = ( NativeLambdaExpr )se;
			applyExpr1.setParent( lambda.getBodyBlock() );
			
			try {
				pushIntoBlock( applyExpr1.getParamBlock() );
			} catch( NotDerivableException e2 ) {
				// can't happen we already know, we have a native lambda expression in front of us
				throw new RuntimeException( e2.getMessage() );
			}
			
			targetNameExpr = lambda.getPrototype().getOutput( channel-1 );
			
			try {
				targetCompoundExpr = currentBlock.getExpr( targetNameExpr );
			}
			catch( NotBoundException e1 ) {
				throw new SemanticModelException( applyExpr.toString(), e1.getMessage() );
			}
			
			try {
				CompoundExpr expr = currentBlock.getExpr( targetNameExpr );
				targetCompoundExpr =  expr.visit( this );
			}
			catch( NotBoundException e ) {}
			
			popBlock();
			
			return targetCompoundExpr;
		}
		
		
		return new CompoundExpr( applyExpr1 );
	}

	@Override
	public CompoundExpr accept(CompoundExpr ce) throws HasFailedException {
		CompoundExpr result, intermediate;
		
		if( ce == null )
			throw new NullPointerException( "Compound expression must not be null." );

		result = new CompoundExpr();
		
		for( SingleExpr singleExpr : ce.getSingleExprList() ) {
			intermediate = singleExpr.visit( this );
			result.addCompoundExpr( intermediate );
		}

		return result;
	}

	@Override
	public CompoundExpr accept(CondExpr condExpr) throws HasFailedException {
		Block thenBlock, thenBlock1, elseBlock, elseBlock1;
		CompoundExpr ce;
		List<NameExpr> outputList;
		CompoundExpr ifExpr;
		boolean maybeNil;
		
		// fetch the then-block of the conditional expression
		thenBlock = condExpr.getThenBlock();
		
		// prepare the then-block of the resulting conditional expression
		thenBlock1 = new Block( thenBlock.getParent() );
		
		// fetch the output variables of this expression
		outputList = condExpr.getPrototype().getOutputList();
		
		for( NameExpr nameExpr : outputList ) {
			
			try {
				ce = thenBlock.getExpr( nameExpr );
			}
			catch( NotBoundException e ) {
				throw new SemanticModelException( condExpr.toString(), e.getMessage() );
			}
			
			ce = ce.visit( this );
			
			thenBlock1.putAssign( nameExpr, ce );
		}
		
		elseBlock = condExpr.getElseBlock();
		
		elseBlock1 = new Block( elseBlock.getParent() );
		
		for( NameExpr nameExpr : outputList ) {
			
			try {
				ce = elseBlock.getExpr( nameExpr );
			}
			catch( NotBoundException e ) {
				throw new SemanticModelException( condExpr.toString(), e.getMessage() );
			}
			
			ce = ce.visit( this );

			elseBlock1.putAssign( nameExpr, ce );
		}
		
		CompoundExpr ifExpr2 = condExpr.getIfExpr();
		ifExpr = ifExpr2.visit( this );
		
		if( ifExpr.getNumSingleExpr() == 0 ) {
			
			// the condition is nil
			
			try {
				return elseBlock1.getExpr( condExpr.getOutputNameExpr() );
			}
			catch( NotBoundException e ) {
				throw new SemanticModelException( condExpr.toString(), e.getMessage() );
			}
		}
			
		maybeNil = true;
		for( SingleExpr se : ifExpr.getSingleExprList() )
			try {
				if( se.getNumAtom() > 0 )
					maybeNil = false;
			}
			catch( NotDerivableException e ) {}
			
		if( maybeNil )

			// we cannot be sure
			
			return new CompoundExpr( new CondExpr(
				condExpr.getChannel(),
				condExpr.getPrototype(),
				ifExpr,
				thenBlock1,
				elseBlock1 ) );
		
		// the condition is true
		
		try {
			return thenBlock.getExpr( condExpr.getOutputNameExpr() );
		}
		catch( NotBoundException e ) {
			throw new SemanticModelException( condExpr.toString(), e.getMessage() );
		}
	}

	@Override
	public CompoundExpr accept( CurryExpr curryExpr ) throws HasFailedException {
		
		Prototype originalPrototype;
		SingleExpr se;
		LambdaExpr lambdaExpr;
		NativeLambdaExpr nativeLambdaExpr;
		Block originalBodyBlock;
		Block curriedBodyBlock;
		Prototype curriedPrototype;
		NativeLambdaExpr curriedLambdaExpr;
		
		try {
		
			if( !curryExpr.hasTaskExpr() )
				throw new SemanticModelException(
					curryExpr.toString(),
					"Task parameter not bound." );
			
			if( curryExpr.getTaskExpr().getNumSingleExpr() == 0 )
				throw new SemanticModelException(
					curryExpr.toString(),
					"Task expression must not be nil." );
			
			if( curryExpr.getTaskExpr().getNumSingleExpr() > 1 )
				return new CompoundExpr( curryExpr );
			
			CompoundExpr taskExpr = curryExpr.getTaskExpr();
			se = taskExpr.visit( this ).getSingleExpr( 0 );
			
			
			if( se instanceof NameExpr )
				return new CompoundExpr( curryExpr );
			
			if( !( se instanceof LambdaExpr ) )
				throw new SemanticModelException( curryExpr.toString(), se+" is not a lambda expression." );
			
			lambdaExpr = ( LambdaExpr )se;
					
			originalPrototype = lambdaExpr.getPrototype();
			
			// the prototype of the curried lambda expression is derived from
			// the original prototype
			curriedPrototype = originalPrototype.clone();
			
			// from the prototype we remove all inputs that are bound by
			// currying
			for( NameExpr nameExpr : curryExpr.getNameSet() )
				curriedPrototype.removeParam( nameExpr );
	
			if( !( lambdaExpr instanceof NativeLambdaExpr ) )
				throw new RuntimeException( "Lambda expression type not recognized." );
				
			nativeLambdaExpr = ( NativeLambdaExpr )lambdaExpr;
			
			originalBodyBlock = nativeLambdaExpr.getBodyBlock();
						
			// the body block of the curried lambda expression is derived from the
			// body block of the original lambda expression
			curriedBodyBlock = originalBodyBlock.clone();
	
			// with the curried expression's binding block merged in
			try {
				for( NameExpr nameExpr : curryExpr.getNameSet() )
					curriedBodyBlock.putAssign(
						nameExpr, curryExpr.getExpr( nameExpr ) );
			}
			catch( NotBoundException e ) {
				throw new RuntimeException( e.getMessage() );
			}			
			
			// from the curried prototype and body expression we form the
			// resulting curried lambda expression
			curriedLambdaExpr = new NativeLambdaExpr( curriedPrototype, curriedBodyBlock );
	
			return new CompoundExpr( curriedLambdaExpr );
		}
		catch( CloneNotSupportedException e ) {
			throw new RuntimeException( e );
		}
		
		// reuse this commented block when in dynamic reducer
		/* if( lambdaExpr instanceof ForeignLambdaExpr ) {
			
			applyExpr = new ApplyExpr( 1, false );
			
			applyExpr.setTaskExpr( new CompoundExpr( lambdaExpr ) );
			
			try {
				
				for( NameExpr nameExpr : originalPrototype.getParamNameExprSet() )
					
					if( curryExpr.containsName( nameExpr ) )
						applyExpr.putAssign( nameExpr, curryExpr.getExpr( nameExpr ) );
					else
						applyExpr.putAssign( nameExpr, new CompoundExpr( nameExpr ) );
			}
			catch( NotBoundException e ) {
				throw new RuntimeException( e.getMessage() );
			}
			
			curriedBodyBlock = new Block();
			
			n = originalPrototype.getNumOutput();
			
			// replicate apply expression for each output name expression
			for( i = 0; i < n; i++  ) {
				
				ne = originalPrototype.getOutput( i );
				
				ae = applyExpr.clone();
				ae.setChannel( i+1 );
				
				curriedBodyBlock.putAssign( ne, new CompoundExpr( ae ) );
			}
			
			curriedLambdaExpr = new NativeLambdaExpr( curriedPrototype, curriedBodyBlock );
			
			return new CompoundExpr( curriedLambdaExpr );			
		} */
	}

	@Override
	public CompoundExpr accept( NameExpr nameExpr ) throws HasFailedException {
		
		CompoundExpr result;
		SingleExpr se;
		CompoundExpr expr;
		
		try {
			
			expr = currentBlock.getExpr( nameExpr );
			result = expr.visit( this );

			if( result.getNumSingleExpr() == 1 ) {
				
				se = result.getSingleExpr( 0 );
				if( se instanceof ForeignLambdaExpr )
					return new CompoundExpr( nameExpr );
			}
			
			return result;
		}
		catch( NotBoundException e ) {
			freeVarSet.add( nameExpr );
			return new CompoundExpr( nameExpr );
		}
	}

	@Override
	public CompoundExpr accept( TopLevelContext tlc ) throws HasFailedException {
		
		CompoundExpr result;
		
		freeVarSet.clear();
		
		result = new CompoundExpr();
		
		for( CompoundExpr ce : tlc.getTargetList() )
			result.addCompoundExpr( ce.visit( this ) );
		
		return result;
	}
	
	public Set<NameExpr> getFreeVarSet() {
		return Collections.unmodifiableSet( freeVarSet );
	}
	
	private void popBlock() {
		currentBlock = blockStack.pop();
	}

	private void pushIntoBlock( Block block ) {
		blockStack.push( currentBlock );
		currentBlock = block;
	}
	
	public static TopLevelContext createTlc( String src ) {
		
		String afterPre, afterChannel;
		TopLevelContext tlc;

		afterPre = PreListener.process( src );
		afterChannel = ChannelListener.process( afterPre );		
		tlc = CfSemanticModelVisitor.process( afterChannel );
		
		return tlc;
	}
	
	public static List<String> getFreeVarNameList( TopLevelContext tlc )throws HasFailedException {
		
		StaticNodeVisitor staticVisitor;
		List<String> nameList;
		
		nameList = new ArrayList<>();
			
		staticVisitor = new StaticNodeVisitor( tlc );
		
		tlc.visit( staticVisitor );
			
		for( NameExpr ne : staticVisitor.getFreeVarSet() )
			nameList.add( ne.getId() );
		
		return Collections.unmodifiableList( nameList );
	}
	
	public static List<String> getTargetVarNameList( TopLevelContext tlc ) {

		List<String> nameList;
		
		nameList = new ArrayList<>();
		
		for( NameExpr ne : tlc.getNameSet() )
			nameList.add( ne.getId() );
		
		return nameList;
	}
}
