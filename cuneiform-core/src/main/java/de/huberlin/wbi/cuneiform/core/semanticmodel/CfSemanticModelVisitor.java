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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.huberlin.wbi.cuneiform.core.parser.CuneiformBaseVisitor;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformLexer;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NameContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.OutputContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ParamBindContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ParamContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.SingleExprContext;
import de.huberlin.wbi.cuneiform.core.preprocess.ParseException;

public class CfSemanticModelVisitor extends CuneiformBaseVisitor<CfNode> implements ANTLRErrorListener {

	public static final String LABEL_TASK = "task";
	public static final String LABEL_FILE = "File";
	
	private BaseBlock currentBlock;
	private final LinkedList<BaseBlock> blockStack;
	private final Log log;
	
	public CfSemanticModelVisitor() {
		
		currentBlock = new TopLevelContext();
		blockStack = new LinkedList<>();
		log = LogFactory.getLog( CfSemanticModelVisitor.class );
	}
	
	@Override
	public CfNode visitApplyExpr( @NotNull CuneiformParser.ApplyExprContext ctx ) {
		
		ApplyExpr applyExpr;
		int channel;
		NameExpr name;
		CfNode compoundExpr;
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
	public CfNode visitAssign( @NotNull CuneiformParser.AssignContext ctx ) {
		
		CfNode nameExpr, compoundExpr;
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
	public CfNode visitBlock( @NotNull CuneiformParser.BlockContext ctx ) {
		
		Block block;
		
		block = new Block( currentBlock );
		
		pushIntoBlock( block );
		visitChildren( ctx );
		popBlock();
		
		return block;
	}
	
	@Override
	public CfNode visitCallExpr( @NotNull CuneiformParser.CallExprContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal call expression encountered." );
	}
	
	@Override
	public CfNode visitIdExpr( @NotNull CuneiformParser.IdExprContext ctx ) { 
		
		return new NameExpr( ctx.ID().getText() );
	}


	@Override
	public CfNode visitCompoundExpr( @NotNull CuneiformParser.CompoundExprContext ctx ) {
		
		CompoundExpr compoundExpr;
		CfNode singleExpr;
		
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
	public CfNode visitCondExpr( @NotNull CuneiformParser.CondExprContext ctx ) {
		
		CfNode ifExpr;
		CfNode thenBlock;
		CfNode elseBlock;
		CfNode prototype;
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
	public CfNode visitCorrelParam( @NotNull CuneiformParser.CorrelParamContext ctx ) {

		CorrelParam correlParam;
		CfNode nameExpr;
		
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
	public CfNode visitCurryExpr( @NotNull CuneiformParser.CurryExprContext ctx ) {
		
		CurryExpr curryExpr;
		String id;
		NameExpr name;
		CfNode compoundExpr;
		
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
	public CfNode visitDanglingExpr( @NotNull CuneiformParser.DanglingExprContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal dangling expression encountered." );
	}
	
	@Override
	public CfNode visitForeignDefTask( @NotNull CuneiformParser.ForeignDefTaskContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal foreign task definition encountered." );
	}
	
	@Override
	public CfNode visitForeignLambdaExpr( @NotNull CuneiformParser.ForeignLambdaExprContext ctx ) {

		CfNode prototype;
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

	
	@Override public CfNode visitFromStackExpr( @NotNull CuneiformParser.FromStackExprContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal from-stack expression encountered." );
	}
	
	@Override
	public CfNode visitImportFile( @NotNull CuneiformParser.ImportFileContext ctx ) {
		throw new RuntimeException( "Illegal import statement encountered." );
	}
	
	@Override
	public CfNode visitIntExpr( @NotNull CuneiformParser.IntExprContext ctx ) {
		return new StringExpr( ctx.getText() );
	}

	
	@Override
	public CfNode visitNameDataType( @NotNull CuneiformParser.NameDataTypeContext ctx ) {		
		return new NameExpr( ctx.ID( 0 ).getText(), new DataType( ctx.ID( 1 ).getText() ) );
	}
	
	@Override
	public CfNode visitNameDeepFnType( @NotNull CuneiformParser.NameDeepFnTypeContext ctx ) {
		
		CfNode prototype;
		
		prototype = visit( ctx.prototype() );
		
		if( prototype == null )
			throw new NullPointerException( "Prototype must not be null." );
		
		if( !( prototype instanceof Prototype ) )
			throw new RuntimeException( "Prototype expected." );
		
		return new NameExpr( ctx.ID().getText(), ( Prototype )prototype );
		
	}
	
	@Override
	public CfNode visitNameInferredType( @NotNull CuneiformParser.NameInferredTypeContext ctx ) {
		return new NameExpr( ctx.ID().getText(), null );
	}
	
	@Override
	public CfNode visitNamePlainFnType( @NotNull CuneiformParser.NamePlainFnTypeContext ctx ) {
		return new NameExpr( ctx.ID().getText(), new LambdaType() );
	}
	
	@Override
	public CfNode visitNativeDefTask( @NotNull CuneiformParser.NativeDefTaskContext ctx ) {
		// sorted out in the pre-phase
		throw new RuntimeException( "Illegal native task definition exncountered." );
	}
	
	@Override
	public CfNode visitNativeLambdaExpr( @NotNull CuneiformParser.NativeLambdaExprContext ctx ) {

		CfNode prototype;
		CfNode block;
		
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
	public CfNode visitNilExpr( @NotNull CuneiformParser.NilExprContext ctx ) {
		return new CompoundExpr();
	}

	@Override
	public CfNode visitPrototype( @NotNull CuneiformParser.PrototypeContext ctx ) {
		
		CfNode node;
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
	public CfNode visitReduceVar( @NotNull CuneiformParser.ReduceVarContext ctx ) {
		
		CfNode nameExpr;
		
		nameExpr = visit( ctx.name() );
		
		if( nameExpr == null )
			throw new NullPointerException( "Name expression must not be null." );
		
		if( !( nameExpr instanceof NameExpr ) )
			throw new RuntimeException( "Name expression expected." );
		
		return ( ( NameExpr )nameExpr ).toReduceVar();
	}

	@Override
	public CfNode visitScript( @NotNull CuneiformParser.ScriptContext ctx ) {
		
		getTopLevelContext().clearTargetList();
		
		visitChildren( ctx );
		return currentBlock;
	}
	
	@Override
	public CfNode visitStringExpr( @NotNull CuneiformParser.StringExprContext ctx ) {
		
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
	public CfNode visitTarget( @NotNull CuneiformParser.TargetContext ctx ) {
		
		CfNode node;
		
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
		CfNode node;
		CfSemanticModelVisitor calculusVisitor;
		
		// parse original content
		instream = new ANTLRInputStream( input );
		
		lexer = new CuneiformLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		parser = new CuneiformParser( tokenStream );
		parser.removeErrorListeners();
		
		calculusVisitor = new CfSemanticModelVisitor();
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
	public void reportAmbiguity( Parser arg0, DFA arg1, int arg2, int arg3,
			boolean arg4, BitSet arg5, ATNConfigSet arg6 ) {
		
		if( log.isDebugEnabled() )
			log.debug( "Ambiguity detected." );

	}

	@Override
	public void reportAttemptingFullContext( Parser arg0, DFA arg1, int arg2,
			int arg3, BitSet arg4, ATNConfigSet arg5 ) {
		
		if( log.isDebugEnabled() )
			log.debug( "Attempting full context." );

	}

	@Override
	public void reportContextSensitivity( Parser arg0, DFA arg1, int arg2,
			int arg3, int arg4, ATNConfigSet arg5) {
		
		if( log.isDebugEnabled() )
			log.debug( "Context sensitivity detected." );
	}

}
