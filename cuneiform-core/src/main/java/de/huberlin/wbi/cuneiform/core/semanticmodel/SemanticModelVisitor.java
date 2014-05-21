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

import java.util.BitSet;
import java.util.LinkedList;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;

import de.huberlin.wbi.cuneiform.core.parser.CuneiformBaseVisitor;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformLexer;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NameContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.OutputContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ParamBindContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ParamContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.SingleExprContext;
import de.huberlin.wbi.cuneiform.core.preprocess.ParseException;

public class SemanticModelVisitor extends CuneiformBaseVisitor<Node> implements ANTLRErrorListener {

	public static final String LABEL_TASK = "task";
	public static final String LABEL_FILE = "File";
	
	private BaseBlock currentBlock;
	private final  LinkedList<BaseBlock> blockStack;
	
	public SemanticModelVisitor() {
		
		currentBlock = new TopLevelContext();
		blockStack = new LinkedList<>();
		
	}
	
	@Override
	public Node visitApplyExpr( @NotNull CuneiformParser.ApplyExprContext ctx ) {
		
		ApplyExpr applyExpr;
		int channel;
		NameExpr name;
		Node compoundExpr;
		String id;
		boolean rest;
		
		if( ctx.channel() == null )
			channel = 1;
		else
			channel = Integer.parseInt( ctx.channel().INT().getText() );
		
		rest = ctx.TILDE() != null;
		applyExpr = new ApplyExpr( channel, rest );
		
		
		for( ParamBindContext pbc : ctx.paramBind() ) {
			
			id = pbc.ID().getText();
			name = new NameExpr( id );
			compoundExpr = visit( pbc.expr() );
			
			if( compoundExpr == null )
				throw new NullPointerException( "Compound expression must not be null." );
			
			if( !( compoundExpr instanceof CompoundExpr ) )
				throw new RuntimeException( "Compound expression expected." );
			
			if( id.equals( LABEL_TASK ) )
				applyExpr.setTaskExpr( ( CompoundExpr )compoundExpr );
			else
				applyExpr.putAssign( name, ( CompoundExpr )compoundExpr );
			
		}
		
		if( !applyExpr.hasTaskExpr() )
			throw new SemanticModelException(
				applyExpr.toString(),
				"Task parameter not bound." );
		
		return applyExpr;
	}

	
	@Override
	public Node visitAssign( @NotNull CuneiformParser.AssignContext ctx ) {
		
		Node nameExpr, compoundExpr;
		CompoundExpr ce;
		SingleExpr se;
		ForeignLambdaExpr lambda;
		
		if( ctx.name().size() != 1 )
			// sorted out in the channel-phase
			throw new RuntimeException(
				"Illegal assignment. Left hand side must have exactly one name element." );
		
		nameExpr = visit( ctx.name( 0 ) );
		
		if( nameExpr == null )
			throw new NullPointerException( "Name expression must not be null." );
		
		if( !( nameExpr instanceof NameExpr ) )
			throw new RuntimeException( "Expected name expression." );
		
		compoundExpr = visit( ctx.expr() );
		
		if( compoundExpr == null )
			throw new NullPointerException( "Compound expression must not be null." );
		
		if( !( compoundExpr instanceof CompoundExpr ) )
			throw new RuntimeException( "Expected compound expression. Found "+compoundExpr.getClass().getName()+"." );
		
		ce = ( CompoundExpr )compoundExpr;
		
		currentBlock.putAssign( ( NameExpr )nameExpr, ce );
		
		// if we assigned a single foreign lambda expression, store name
		if( ce.getNumSingleExpr() == 1 ) {
			
			se = ce.getSingleExpr( 0 );
			if( se instanceof ForeignLambdaExpr ) {
				
				lambda = ( ForeignLambdaExpr )se;
				lambda.setTaskName( ( ( NameExpr )nameExpr ).getId() );
			}
				
		}
		
		return currentBlock;
	}
	
	@Override
	public Node visitBlock( @NotNull CuneiformParser.BlockContext ctx ) {
		
		Block block;
		
		block = new Block( currentBlock );
		
		pushIntoBlock( block );
		visitChildren( ctx );
		popBlock();
		
		return block;
	}
	
	@Override
	public Node visitCallExpr( @NotNull CuneiformParser.CallExprContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal call expression encountered." );
	}
	
	@Override
	public Node visitIdExpr( @NotNull CuneiformParser.IdExprContext ctx ) { 
		
		return new NameExpr( ctx.ID().getText() );
	}


	@Override
	public Node visitCompoundExpr( @NotNull CuneiformParser.CompoundExprContext ctx ) {
		
		CompoundExpr compoundExpr;
		Node singleExpr;
		
		compoundExpr = new CompoundExpr();
				
		for( SingleExprContext sec : ctx.singleExpr() ) {	
			
			singleExpr = visit( sec );
						
			if( singleExpr == null )
				throw new NullPointerException( "While evaluating compound expression '"+sec.getText()+"': Single expression must not be null." );
				
			
			if( !( singleExpr instanceof SingleExpr ) )
				throw new RuntimeException( "Single expresssion expected." );
			
			compoundExpr.addSingleExpr( ( SingleExpr )singleExpr );
		}
		
		return compoundExpr;
	}
	
	@Override
	public Node visitCondExpr( @NotNull CuneiformParser.CondExprContext ctx ) {
		
		Node ifExpr;
		Node thenBlock;
		Node elseBlock;
		Node prototype;
		int channel;
		
		if( ctx.channel() == null )
			throw new NullPointerException( "No channel information in conditional expression." );
		
		channel = Integer.parseInt( ctx.channel().INT().getText() );
		
		prototype = visit( ctx.prototype() );
		
		if( prototype == null )
			throw new NullPointerException( "Prototype must not be null." );
		
		if( !( prototype instanceof Prototype ) )
			throw new RuntimeException( "Prototype expected." );
		
		if( ( ( Prototype )prototype ).getNumParam() > 0 )
			throw new SemanticModelException(
				prototype,
				"Conditional prototype must not define input parameters." );
		
		for( NameExpr output : ( ( Prototype )prototype ).getOutputList() )
			if( !( output instanceof ReduceVar ) )
				throw new SemanticModelException(
					prototype,
					"Non-reduce output in conditional prototype." );
		
		ifExpr = visit( ctx.expr() );
		
		if( ifExpr == null )
			throw new NullPointerException( "If expression must not be null." );
		
		if( !( ifExpr instanceof CompoundExpr ) )
			throw new RuntimeException( "Compound expression expected." );
		
		thenBlock = visit( ctx.block( 0 ) );
		
		if( thenBlock == null )
			throw new NullPointerException( "Then block must not be null." );
		
		if( !( thenBlock instanceof Block ) )
			throw new RuntimeException( "Block expected." );
		
		elseBlock = visit( ctx.block( 1 ) );
		
		if( elseBlock == null )
			throw new NullPointerException( "Then block must not be null." );
		
		if( !( elseBlock instanceof Block ) )
			throw new RuntimeException( "Block expected." );
		
		return new CondExpr( channel,
			( Prototype )prototype, ( CompoundExpr )ifExpr,
			( Block )thenBlock, ( Block )elseBlock );
	}

	
	@Override
	public Node visitCorrelParam( @NotNull CuneiformParser.CorrelParamContext ctx ) {

		CorrelParam correlParam;
		Node nameExpr;
		
		correlParam = new CorrelParam();
		
		for( NameContext nc : ctx.name() ) {
			
			nameExpr = visit( nc );
			
			if( nameExpr == null )
				throw new NullPointerException( "Name expression must not be null." );
			
			if( !( nameExpr instanceof NameExpr ) )
				throw new RuntimeException( "Name expression expected." );
			
			correlParam.addName( ( NameExpr )nameExpr );
		}
		
		return correlParam;
	}
	@Override
	public Node visitCurryExpr( @NotNull CuneiformParser.CurryExprContext ctx ) {
		
		CurryExpr curryExpr;
		String id;
		NameExpr name;
		Node compoundExpr;
		
		curryExpr = new CurryExpr();
		
		for( ParamBindContext pbc : ctx.paramBind() ) {
			
			id = pbc.ID().getText();
			name = new NameExpr( id );
			compoundExpr = visit( pbc.expr() );
			
			if( compoundExpr == null )
				throw new NullPointerException( "Compound expression must not be null." );
			
			if( !( compoundExpr instanceof CompoundExpr ) )
				throw new RuntimeException( "Compound expression expected." );
			
			if( id.equals( LABEL_TASK ) )
				curryExpr.setTaskExpr( ( CompoundExpr )compoundExpr );
			else
				curryExpr.putAssign( name, ( CompoundExpr )compoundExpr );
			
		}
		
		if( !curryExpr.hasTaskExpr() )
			throw new SemanticModelException(
				curryExpr.toString(),
				"Task parameter not bound." );
		
		return curryExpr;
	}
	
	@Override
	public Node visitDanglingExpr( @NotNull CuneiformParser.DanglingExprContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal dangling expression encountered." );
	}
	
	@Override
	public Node visitForeignDefTask( @NotNull CuneiformParser.ForeignDefTaskContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal foreign task definition encountered." );
	}
	
	@Override
	public Node visitForeignLambdaExpr( @NotNull CuneiformParser.ForeignLambdaExprContext ctx ) {

		Node prototype;
		String langString;
		String body;
		
		prototype = visit( ctx.prototype() );
		
		if( prototype == null )
			throw new NullPointerException( "Prototype must not be null." );
		
		if( !( prototype instanceof Prototype ) )
			throw new RuntimeException( "Prototype expected." );
		
		langString = ctx.foreignBody().ID().getText();
		
		body = ctx.foreignBody().BODY().getText();
		body = body.substring( 2, body.length()-2 );
		
		if( body.trim().isEmpty() )
			throw new SemanticModelException(
				prototype+"in "+langString+" *{}*",
				"Foreign body block must not be empty." );
		
		for( NameExpr name : ( ( Prototype )prototype ).getOutputList() )
			if( !body.contains( name.getId() ) )
				throw new SemanticModelException( prototype+"in "+langString+" *{ "+body+" }*", "Output variable "+name.getId()+" is never bound." );
		
		return new ForeignLambdaExpr( ( Prototype )prototype, langString, body );
	}

	
	@Override public Node visitFromStackExpr( @NotNull CuneiformParser.FromStackExprContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal from-stack expression encountered." );
	}
	
	@Override
	public Node visitImportFile( @NotNull CuneiformParser.ImportFileContext ctx ) {
		throw new RuntimeException( "Illegal import statement encountered." );
	}
	
	@Override
	public Node visitIntExpr( @NotNull CuneiformParser.IntExprContext ctx ) {
		return new StringExpr( ctx.getText() );
	}

	
	@Override
	public Node visitNameDataType( @NotNull CuneiformParser.NameDataTypeContext ctx ) {		
		return new NameExpr( ctx.ID( 0 ).getText(), new DataType( ctx.ID( 1 ).getText() ) );
	}
	
	@Override
	public Node visitNameDeepFnType( @NotNull CuneiformParser.NameDeepFnTypeContext ctx ) {
		
		Node prototype;
		
		prototype = visit( ctx.prototype() );
		
		if( prototype == null )
			throw new NullPointerException( "Prototype must not be null." );
		
		if( !( prototype instanceof Prototype ) )
			throw new RuntimeException( "Prototype expected." );
		
		return new NameExpr( ctx.ID().getText(), ( Prototype )prototype );
		
	}
	
	@Override
	public Node visitNameInferredType( @NotNull CuneiformParser.NameInferredTypeContext ctx ) {
		return new NameExpr( ctx.ID().getText(), null );
	}
	
	@Override
	public Node visitNamePlainFnType( @NotNull CuneiformParser.NamePlainFnTypeContext ctx ) {
		return new NameExpr( ctx.ID().getText(), new LambdaType() );
	}
	
	@Override
	public Node visitNativeDefTask( @NotNull CuneiformParser.NativeDefTaskContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal native task definition exncountered." );
	}
	
	@Override
	public Node visitNativeLambdaExpr( @NotNull CuneiformParser.NativeLambdaExprContext ctx ) {

		Node prototype;
		Node block;
		
		prototype = visit( ctx.prototype() );
		
		if( prototype == null )
			throw new NullPointerException( "Prototype must not be null." );
		
		if( !( prototype instanceof Prototype ) )
			throw new RuntimeException( "Prototype expected." );
		
		block = visit( ctx.block() );
		
		if( block == null )
			throw new NullPointerException( "Symbol table must not be null." );
		
		if( !( block instanceof Block ) )
			throw new RuntimeException( "Symbol table expected." );
		
		return new NativeLambdaExpr( ( Prototype )prototype, ( Block )block );
	}

	
	@Override
	public Node visitNilExpr( @NotNull CuneiformParser.NilExprContext ctx ) {
		return new CompoundExpr();
	}

	@Override
	public Node visitPrototype( @NotNull CuneiformParser.PrototypeContext ctx ) {
		
		Node node;
		Prototype prototype;
		
		prototype = new Prototype();
		
		for( OutputContext oc : ctx.output() ) {
			
			node = visit( oc );
			
			if( node == null )
				throw new NullPointerException( "Output must not be null." );
			
			if( !( node instanceof NameExpr ) )
				throw new RuntimeException( "Output expected." );
			
			prototype.addOutput( ( NameExpr )node );
		}
		
		for( ParamContext pc : ctx.param() ) {
			
			node = visit( pc );
			
			if( node == null )
				throw new NullPointerException( "Parameter must not be null." );
			
			if( !( node instanceof Param ) )
				throw new RuntimeException( "Parameter expected." );
			
			prototype.addParam( ( Param )node );
		}
		
		return prototype;
	}

	@Override
	public Node visitReduceVar( @NotNull CuneiformParser.ReduceVarContext ctx ) {
		
		Node nameExpr;
		
		nameExpr = visit( ctx.name() );
		
		if( nameExpr == null )
			throw new NullPointerException( "Name expression must not be null." );
		
		if( !( nameExpr instanceof NameExpr ) )
			throw new RuntimeException( "Name expression expected." );
		
		return ( ( NameExpr )nameExpr ).toReduceVar();
	}

	@Override
	public Node visitScript( @NotNull CuneiformParser.ScriptContext ctx ) {
		
		getTopLevelContext().clearTargetList();
		
		visitChildren( ctx );
		return currentBlock;
	}
	
	@Override
	public Node visitStringExpr( @NotNull CuneiformParser.StringExprContext ctx ) {
		
		String content;
		
		content = ctx.getText();
		
		if( content.startsWith( "\"" ) )
			content = content.replace( "\\\"", "\"" );
		else
			content = content.replace( "\\'", "'" );
		
		content = content.substring( 1, content.length()-1 );
		content = content.replace( "\\\\", "\\" );
		
		return new StringExpr( content );
	}

	
	@Override
	public Node visitTarget( @NotNull CuneiformParser.TargetContext ctx ) {
		
		Node node;
		
		node = visit( ctx.expr() );
		
		if( node == null )
			throw new NullPointerException( "Compound expression must not be null." );
		
		if( !( node instanceof CompoundExpr ) )
			throw new RuntimeException( "Expected compound expression." );
		
		if( currentBlock == null )
			throw new NullPointerException( "Current symbol table must not be null." );
		
		if( !( currentBlock instanceof TopLevelContext ) )
			throw new RuntimeException( "Block expected." );
		
		( ( TopLevelContext )currentBlock ).addTarget( ( CompoundExpr )node );
		
		return node;	
	}
	
	private void pushIntoBlock( Block block ) {
		
		blockStack.push( currentBlock );
		currentBlock = block;
	}
	
	private void popBlock() {
		currentBlock = blockStack.pop();
	}
	
	public TopLevelContext getTopLevelContext() {
		
		if( !( currentBlock instanceof TopLevelContext ) )
			throw new RuntimeException( "Not in top level context." );
		
		return ( TopLevelContext )currentBlock;
	}

	public static TopLevelContext process( String input ) {
		
		ANTLRInputStream instream;
		CuneiformLexer lexer;
		CommonTokenStream tokenStream;
		CuneiformParser parser;
		ParseTree tree;
		Node node;
		SemanticModelVisitor calculusVisitor;
		
		// parse original content
		instream = new ANTLRInputStream( input );
		
		lexer = new CuneiformLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		parser = new CuneiformParser( tokenStream );
		parser.removeErrorListeners();
		
		calculusVisitor = new SemanticModelVisitor();
		lexer.addErrorListener( calculusVisitor );
		parser.addErrorListener( calculusVisitor );
		
		tree = parser.script();
		
		node = calculusVisitor.visit( tree );
		
		if( node == null )
			throw new NullPointerException( "Node must not be null." );
		
		if( !( node instanceof TopLevelContext ) )
			throw new RuntimeException( "Top level context expected." );
		
		return ( TopLevelContext )node;
	}
	
	@Override
	public void syntaxError(Recognizer<?, ?> recognizer,
			Object offendingSymbol, int line, int charPositionInLine,
			String msg, RecognitionException e) {
		throw new ParseException( line, charPositionInLine, ( ( Token )offendingSymbol ).getText(), msg );
		
	}

	@Override
	public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex,
			int stopIndex, boolean exact, BitSet ambigAlts, ATNConfigSet configs) {
		// throw new RuntimeException( "Ambiguity detected." );
	}

	@Override
	public void reportAttemptingFullContext(Parser recognizer, DFA dfa,
			int startIndex, int stopIndex, BitSet conflictingAlts,
			ATNConfigSet configs) {
		// throw new RuntimeException( "Attempting full context." );
	}

	@Override
	public void reportContextSensitivity(Parser recognizer, DFA dfa,
			int startIndex, int stopIndex, int prediction, ATNConfigSet configs) {
		// throw new RuntimeException( "Context sensitivity detected." );
	}
}
