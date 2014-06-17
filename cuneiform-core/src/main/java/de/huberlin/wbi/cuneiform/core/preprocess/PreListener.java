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

package de.huberlin.wbi.cuneiform.core.preprocess;

import java.util.BitSet;
import java.util.LinkedList;
import java.util.List;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStreamRewriter;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.huberlin.wbi.cuneiform.core.parser.CuneiformBaseListener;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformLexer;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.CondExprContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.CorrelParamContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.PrototypeContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NameContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NameDataTypeContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NameDeepFnTypeContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NameInferredTypeContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NamePlainFnTypeContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ParamContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ReduceVarContext;

/** Simplifies token stream by removing syntactic sugar.
 * 
 * - Replaces deftask statements with assignments to lambda expressions
 * - Replaces task calls with apply expressions
 * - Resolves pipe notation to regular nested function calls
 * - Removes comments
 * - Explicitly references channels in applications
 * - Adds explicit task parameter to all task prototypes
 * - Quotes numbers
 * 
 * @author jorgen
 *
 */
public class PreListener extends CuneiformBaseListener implements ANTLRErrorListener {

	private final TokenStreamRewriter rewriter;
	private final LinkedList<String> exprStack;
	private final Log log;
	
	public PreListener( CommonTokenStream tokenStream ) {
		
		if( tokenStream == null )
			throw new NullPointerException( "Token stream must not be empty." );
				
		rewriter = new TokenStreamRewriter( tokenStream );
		exprStack = new LinkedList<>();
		log = LogFactory.getLog( PreListener.class );
	}
	
	public static String process( String input ) {
		
		ANTLRInputStream instream;
		CuneiformLexer lexer;
		CommonTokenStream tokenStream;
		CuneiformParser parser;
		ParseTree tree;
		ParseTreeWalker walker;
		PreListener preListener;
		
		
		walker = new ParseTreeWalker();
		
		// parse original content
		instream = new ANTLRInputStream( input );
		
		lexer = new CuneiformLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		parser = new CuneiformParser( tokenStream );
		parser.removeErrorListeners();
		
		preListener = new PreListener( tokenStream );
		lexer.addErrorListener( preListener );
		parser.addErrorListener( preListener );

		
		tree = parser.script();
		
		walker.walk( preListener, tree );
		
		return preListener.getRewrittenText();
	}
	
	@Override
	public void enterCallExpr( @NotNull CuneiformParser.CallExprContext ctx ) {

		Token id;
		Token lparen;
		
		id = ctx.ID().getSymbol();
		lparen = ctx.LPAREN().getSymbol();
		
		rewriter.replace( id, "apply" );
		
		rewriter.insertAfter( lparen, " task: "+id.getText()+" " );
	}
	
	@Override
	public void enterForeignDefTask( @NotNull CuneiformParser.ForeignDefTaskContext ctx ) {
		
		Token deftask;
		Token id;
		Token bodyStop;
		
		deftask = ctx.DEFTASK().getSymbol();
		bodyStop = ctx.foreignBody().getStop();
		id = ctx.ID().getSymbol();
				
		rewriter.insertAfter( bodyStop, ";" );
		rewriter.insertAfter( id, " = \\" );
		rewriter.replace( deftask, id, id.getText() );
		
	}
	
	@Override
	public void enterNativeDefTask( @NotNull CuneiformParser.NativeDefTaskContext ctx ) {
		
		Token deftask;
		Token id;
		Token bodyStop;
		
		deftask = ctx.DEFTASK().getSymbol();
		bodyStop = ctx.RBRACE().getSymbol();
		id = ctx.ID().getSymbol();
				
		rewriter.insertAfter( bodyStop, ";" );
		rewriter.insertAfter( id, " = \\" );
		rewriter.replace( deftask, id, id.getText() );
		
	}
	
	@Override
	public void exitDanglingExpr( @NotNull CuneiformParser.DanglingExprContext ctx ) {
		exprStack.push( rewriter.getText( ctx.expr().getSourceInterval() ) );
		rewriter.delete( ctx.getStart(), ctx.getStop() );
	}
	
	@Override
	public void enterPrototype( @NotNull CuneiformParser.PrototypeContext ctx ) {
		
		
		if( !( ctx.getParent() instanceof CondExprContext ) )
			if( !containsParam( ctx, "task" ) )
				rewriter.insertAfter( ctx.COLON().getSymbol(), " task " );
	}
		
	@Override
	public void enterScript( @NotNull CuneiformParser.ScriptContext ctx ) {
		
		if( rewriter == null )
			throw new NullPointerException( "Token stream not set." );
	}
	
	@Override
	public void exitFromStackExpr( @NotNull CuneiformParser.FromStackExprContext ctx ) {
		rewriter.replace( ctx.FROMSTACK().getSymbol(), exprStack.pop() );
	}
	
	public String getRewrittenText() {
		return rewriter.getText();
	}
	
	public static boolean containsParam( PrototypeContext prototype, String paramName ) {
		
		List<String> nameList;
		
		nameList = getNameList( prototype );
		return nameList.contains( paramName );
	}
	
	public static List<String> getNameList( PrototypeContext prototype ) {

		List<String> nameList;
		
		nameList = new LinkedList<>();
		for( ParamContext param : prototype.param() )
			nameList.addAll( getNameList( param ) );

		return nameList;
	}
	
	public static List<String> getNameList( ParamContext param ) {
		
		List<String> list;
		
		list = new LinkedList<>();
		
		if( param.name() != null ) {
			list.add( getName( param.name() ) );
			return list;
		}

		if( param.reduceVar() != null ) {			
			list.add( getName( param.reduceVar() ) );
			return list;
		}
		
		if( param.correlParam() != null ) {
			list.addAll( getNameList( param.correlParam() ) );
			return list;
		}
		
		throw new RuntimeException( "Parameter type not recognized." );
						
	}
	
	public static String getName( NameContext name ) {
		
		if( name == null )
			throw new NullPointerException( "Name must not be null." );
		
		if( name instanceof NameInferredTypeContext )
			return ( ( NameInferredTypeContext )name ).ID().getText();
		
		if( name instanceof NameDataTypeContext )
			return ( ( NameDataTypeContext )name ).ID( 0 ).getText();
		
		if( name instanceof NamePlainFnTypeContext )
			return ( ( NamePlainFnTypeContext )name ).ID().getText();
		
		if( name instanceof NameDeepFnTypeContext )
			return ( ( NameDeepFnTypeContext )name ).ID().getText();
		
		throw new RuntimeException( "Name type "+name.getClass().getName()+" not recognized. In '"+name.getText()+"'" );
	}
	
	public static String getName( ReduceVarContext reduceVar ) {
		return getName( reduceVar.name() );
	}
	
	public static List<String> getNameList( CorrelParamContext correlVar ) {
		
		List<String> list;
		
		list = new LinkedList<>();
		
		for( NameContext nc : correlVar.name() )
			list.add( getName( nc ) );
		
		return list;
	}

	@Override
	public void syntaxError( Recognizer<?, ?> recognizer,
			Object offendingSymbol, int line, int charPositionInLine,
			String msg, RecognitionException e ) {
		
		String near;
		
		near = null;
		if( offendingSymbol != null )
			near = ( ( Token )offendingSymbol ).getText();
		
		throw new ParseException( line, charPositionInLine, near, msg );
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
		
		if( log.isTraceEnabled() )
			log.trace( "Attempting full context." );

	}

	@Override
	public void reportContextSensitivity( Parser arg0, DFA arg1, int arg2,
			int arg3, int arg4, ATNConfigSet arg5) {
		
		if( log.isTraceEnabled() )
			log.trace( "Context sensitivity detected." );
	}

	

	

}
