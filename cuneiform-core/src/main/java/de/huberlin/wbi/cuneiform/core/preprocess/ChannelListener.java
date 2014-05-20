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
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.TokenStreamRewriter;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import de.huberlin.wbi.cuneiform.core.parser.CuneiformBaseListener;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformLexer;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ApplyExprContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.CallExprContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.CompoundExprContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.CondExprContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.ExprContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NameContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.NilExprContext;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser.SingleExprContext;

public class ChannelListener extends CuneiformBaseListener implements ANTLRErrorListener {

	private TokenStreamRewriter rewriter;
	
	public ChannelListener( CommonTokenStream tokenStream ) {
		setTokenStream( tokenStream );
	}
	
	@Override
	public void enterAssign( @NotNull CuneiformParser.AssignContext ctx ) {
		
		List<SingleExprContext> unqual;
		int i, n;
		StringBuffer buf;
		TokenStream stream;
		NameContext name;
		String nameString;
		
		if( rewriter == null )
			throw new NullPointerException( "Token stream has never been set." );
		
		stream = rewriter.getTokenStream();
		unqual = getUnqualifiedApplicationList( ctx.expr() );
		
		
		n = ctx.name().size();
				
		for( i = n-1; i > 0; i-- ) {
			
			buf = new StringBuffer();
			name = ctx.name( i );
			
			buf.append( '\n' );
			buf.append( stream.getText( name.getStart(), name.getStop() ) );
			buf.append( " =" );
			
			if( unqual.isEmpty() )
				buf.append( " nil" );
			else			
				for( SingleExprContext se : unqual )
					
					buf.append( " [" ).append( i+1 ).append( "]" )
						.append( stream.getText( se.getStart(), se.getStop() ) );
			
			
			
			buf.append( ';' );
			
			rewriter.insertAfter( ctx.getStop(), buf.toString() );	
		}
		
		name = ctx.name( 0 );
		nameString = stream.getText( name.getStart(), name.getStop() );
		
		rewriter.replace( name.getStart(), ctx.EQUAL().getSymbol(), nameString+" =" );
	}
	
	@Override
	public void enterCallExpr( @NotNull CuneiformParser.CallExprContext ctx ) {
		throw new RuntimeException( "Call expression should have been removed." );
	}
	
	@Override
	public void enterCondExpr( @NotNull CuneiformParser.CondExprContext ctx ) {
		
		if( rewriter == null )
			throw new NullPointerException( "Token stream has never been set." );
		
		if( ctx.channel() == null )
			rewriter.insertBefore( ctx.getStart(), "[1]" );
	}
	
	@Override
	public void enterScript( @NotNull CuneiformParser.ScriptContext ctx ) {
		
		if( rewriter == null )
			throw new NullPointerException( "Token stream not set." );
	}
	
	public String getRewrittenText() {
		
		if( rewriter == null )
			throw new NullPointerException( "Token stream not set." );
		
		return rewriter.getText();
	}
	
	public void setTokenStream( CommonTokenStream tokenStream ) {
		
		if( tokenStream == null )
			throw new NullPointerException( "Token stream must not be empty." );
		
		rewriter = new TokenStreamRewriter( tokenStream );
	}

	public static String process( String input ) {

		ANTLRInputStream instream;
		CuneiformLexer lexer;
		CommonTokenStream tokenStream;
		CuneiformParser parser;
		ParseTree tree;
		ParseTreeWalker walker;
		ChannelListener channelListener;
					
		walker = new ParseTreeWalker();
		
		
		// parse original content
		instream = new ANTLRInputStream( input );
		
		lexer = new CuneiformLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		parser = new CuneiformParser( tokenStream );
		parser.removeErrorListeners();
		
		channelListener = new ChannelListener( tokenStream );
		lexer.addErrorListener( channelListener );
		parser.addErrorListener( channelListener );

		tree = parser.script();
				
		walker.walk( channelListener, tree );
		
		return channelListener.getRewrittenText();
	}
	
	public static List<SingleExprContext> getUnqualifiedApplicationList( ExprContext expr ) {
		
		List<SingleExprContext> unqualifiedList;
		CompoundExprContext cec;
		
		if( expr == null )
			throw new NullPointerException( "Expression must not be null." );
		
		unqualifiedList = new LinkedList<>();
		
		if( expr instanceof NilExprContext )
			return unqualifiedList;
		
		if( expr instanceof CompoundExprContext ) {
			
			cec = ( CompoundExprContext )expr;
		
			for( SingleExprContext singleExpr : cec.singleExpr() )
				if( isUnqualifiedApplication( singleExpr ) )
					unqualifiedList.add( singleExpr );
		
			return unqualifiedList;	
		}
		
		throw new RuntimeException( "Expression type not recognized." );
	}
	
	public static boolean isUnqualifiedApplication( SingleExprContext singleExpr ) {
		
		if( singleExpr instanceof ApplyExprContext )
			
			if( ( ( ApplyExprContext )singleExpr ).channel() == null )
				return true;
					
		
		if( singleExpr instanceof CallExprContext )
			
			if( ( ( CallExprContext )singleExpr ).channel() == null )
				return true;
		
		if( singleExpr instanceof CondExprContext )
			
			if( ( ( CondExprContext )singleExpr ).channel() == null )
				return true;
		
		return false;
	}

	@Override
	public void syntaxError( Recognizer<?, ?> recognizer,
			Object offendingSymbol, int line, int charPositionInLine,
			String msg, RecognitionException e ) {
		throw new ParseException( line, charPositionInLine, ( ( Token )offendingSymbol ).getText(), msg );
	}

	@Override
	public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex,
			int stopIndex, boolean exact, BitSet ambigAlts, ATNConfigSet configs) {
	}

	@Override
	public void reportAttemptingFullContext(Parser recognizer, DFA dfa,
			int startIndex, int stopIndex, BitSet conflictingAlts,
			ATNConfigSet configs) {
	}

	@Override
	public void reportContextSensitivity(Parser recognizer, DFA dfa,
			int startIndex, int stopIndex, int prediction, ATNConfigSet configs) {
	}
	

}
