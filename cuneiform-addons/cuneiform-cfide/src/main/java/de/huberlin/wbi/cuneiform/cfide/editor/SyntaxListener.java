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

package de.huberlin.wbi.cuneiform.cfide.editor;


import javax.swing.JTextPane;
import javax.swing.text.Style;
import javax.swing.text.StyledDocument;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;

import de.huberlin.wbi.cuneiform.core.parser.CuneiformBaseListener;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformLexer;
import de.huberlin.wbi.cuneiform.core.parser.CuneiformParser;

public class SyntaxListener extends CuneiformBaseListener {
		
	private JTextPane textPane;
	
	public SyntaxListener( JTextPane textPane, StyleConf conf ) {
			
		if( textPane == null )
			throw new NullPointerException( "Text pane must not be null." );
		
		if( conf == null )
			throw new NullPointerException( "Style configuration object never set." );
				
		this.textPane = textPane;
		conf.setDoc( getDoc() );
		
	}
	
	public StyledDocument getDoc() {
		return textPane.getStyledDocument();
	}
	
	public static void process( JTextPane textPane, StyleConf conf ) {
		
		ANTLRInputStream instream;
		CuneiformLexer lexer;
		TokenStream tokenStream;
		CuneiformParser parser;
		ParseTree tree;
		SyntaxListener syntaxListener;
		ParseTreeWalker walker;

		walker = new ParseTreeWalker();
		
		
		// parse content
		instream = new ANTLRInputStream( textPane.getText() );
		
		lexer = new CuneiformLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		parser = new CuneiformParser( tokenStream );
		parser.removeErrorListeners();

		syntaxListener = new SyntaxListener( textPane, conf );
		
		tree = parser.script();
		walker.walk( syntaxListener, tree );		
	}
		
	@Override
	public void enterNilExpr( @NotNull CuneiformParser.NilExprContext ctx ) {		
		mark( ctx, StyleConf.KEY_KEYWORD );
		mark( ctx, StyleConf.KEY_DATA );
	}
	
	@Override
	public void enterStat( @NotNull CuneiformParser.StatContext ctx ) {
		mark( ctx, StyleConf.KEY_STAT, true );
	}
	
	@Override
	public void enterScript( @NotNull CuneiformParser.ScriptContext ctx ) {
		
		StyledDocument doc;
		
		doc = getDoc();
		
		doc.setCharacterAttributes(
			0,
			doc.getLength(),
			doc.getStyle( StyleConf.KEY_COMMENT ),
			true );
	}
	
	@Override
	public void enterTarget( @NotNull CuneiformParser.TargetContext ctx ) {
		mark( ctx, StyleConf.KEY_APPLY );
	}
	
	@Override
	public void enterNativeDefTask( @NotNull CuneiformParser.NativeDefTaskContext ctx ) {
		mark( ctx.DEFTASK(), StyleConf.KEY_KEYWORD );
		mark( ctx.ID(), StyleConf.KEY_CALL );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );
	}
	
	@Override
	public void enterForeignDefTask( @NotNull CuneiformParser.ForeignDefTaskContext ctx ) {
		mark( ctx.DEFTASK(), StyleConf.KEY_KEYWORD );
		mark( ctx.ID(), StyleConf.KEY_CALL );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );
	}
	
	@Override
	public void enterDefTaskErr1( @NotNull CuneiformParser.DefTaskErr1Context ctx ) {
		mark( ctx.DEFTASK(), StyleConf.KEY_KEYWORD );		
	}
	
	@Override
	public void enterDefTaskErr2( @NotNull CuneiformParser.DefTaskErr2Context ctx ) {
		mark( ctx.DEFTASK(), StyleConf.KEY_KEYWORD );		
		mark( ctx.ID(), StyleConf.KEY_CALL );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );		
	}

	@Override
	public void enterDefTaskErr3( @NotNull CuneiformParser.DefTaskErr3Context ctx ) {
		mark( ctx.DEFTASK(), StyleConf.KEY_KEYWORD );		
		mark( ctx.ID(), StyleConf.KEY_CALL );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );	
	}
	
	@Override
	public void enterFnPrototypeErr1( @NotNull CuneiformParser.FnPrototypeErr1Context ctx ) {		
		mark( ctx.DEFTASK(), StyleConf.KEY_KEYWORD );		
		mark( ctx.ID(), StyleConf.KEY_CALL );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );	
	}

	@Override
	public void enterFnPrototypeErr2( @NotNull CuneiformParser.FnPrototypeErr2Context ctx ) {
		mark( ctx.DEFTASK(), StyleConf.KEY_KEYWORD );		
		mark( ctx.ID(), StyleConf.KEY_CALL );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );		
	}
	
	@Override
	public void enterForeignBody( @NotNull CuneiformParser.ForeignBodyContext ctx ) {
		mark( ctx.INLANG(), StyleConf.KEY_KEYWORD );
		mark( ctx.BODY(), StyleConf.KEY_FOREIGN );
	}	
	
	@Override
	public void enterForeignFnBodyErr2( @NotNull CuneiformParser.ForeignFnBodyErr2Context ctx ) {
		mark( ctx.INLANG(), StyleConf.KEY_KEYWORD );		
	}

	@Override
	public void enterNameInferredType( @NotNull CuneiformParser.NameInferredTypeContext ctx ) {
		mark( ctx.ID(), StyleConf.KEY_VARNAME );
	}
	
	@Override
	public void enterNameDataType( @NotNull CuneiformParser.NameDataTypeContext ctx ) {
		mark( ctx, StyleConf.KEY_TYPE );
		mark( ctx.ID( 0 ), StyleConf.KEY_VARNAME );
	}

	@Override public void enterNameDeepFnType( @NotNull CuneiformParser.NameDeepFnTypeContext ctx ) {
		mark( ctx, StyleConf.KEY_TYPE );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );
	}
	
	@Override
	public void enterNamePlainFnType( @NotNull CuneiformParser.NamePlainFnTypeContext ctx ) {
		mark( ctx, StyleConf.KEY_TYPE );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );		
	}
	
	@Override
	public void enterIntExpr( @NotNull CuneiformParser.IntExprContext ctx ) {
		mark( ctx, StyleConf.KEY_DATA );
	}
	
	@Override
	public void enterStringExpr( @NotNull CuneiformParser.StringExprContext ctx ) {
		mark( ctx, StyleConf.KEY_DATA );
	}
	
	@Override
	public void enterApplyExpr( @NotNull CuneiformParser.ApplyExprContext ctx ) {
		mark( ctx.APPLY(), StyleConf.KEY_KEYWORD );
		mark( ctx.APPLY(), StyleConf.KEY_CALL );
	}
	
	@Override
	public void enterCallExpr( @NotNull CuneiformParser.CallExprContext ctx ) {
		mark( ctx.ID(), StyleConf.KEY_CALL );
		mark( ctx.ID(), StyleConf.KEY_VARNAME );
	}
	
	@Override public void enterCurryExpr(@NotNull CuneiformParser.CurryExprContext ctx) {
		mark( ctx.CURRY(), StyleConf.KEY_KEYWORD );
		mark( ctx.CURRY(), StyleConf.KEY_CALL );
	}
	
	@Override
	public void enterNativeLambdaExpr( @NotNull CuneiformParser.NativeLambdaExprContext ctx ) {
		mark( ctx.LAMBDA(), StyleConf.KEY_KEYWORD );
	}
	
	@Override
	public void enterForeignLambdaExpr( @NotNull CuneiformParser.ForeignLambdaExprContext ctx ) {
		mark( ctx.LAMBDA(), StyleConf.KEY_KEYWORD );
	}
	
	@Override
	public void enterParamBind( @NotNull CuneiformParser.ParamBindContext ctx ) {
		mark( ctx.ID(), StyleConf.KEY_VARNAME );
	}
	
	@Override
	public void enterDanglingExpr( @NotNull CuneiformParser.DanglingExprContext ctx ) {
		mark( ctx.TOSTACK(), StyleConf.KEY_COMMENT );
	}
	
	@Override
	public void enterFromStackExpr( @NotNull CuneiformParser.FromStackExprContext ctx ) {
		mark( ctx.FROMSTACK(), StyleConf.KEY_COMMENT );
	}
		
	private void mark( TerminalNode tn, String styleName ) {
		
		Token sym;
		
		if( tn == null )
			return;
		
		sym = tn.getSymbol();
		if( sym == null )
			return;
		
		mark( sym, styleName );
	}
	
	private void mark( Token t, String styleName ) {
		
		Style style;
		int start, len;
		StyledDocument doc;
		
		if( t == null )
			return;
		
		if( styleName == null )
			throw new NullPointerException( "Style name must not be null." );
		
		if( styleName.isEmpty() )
			throw new RuntimeException( "Style name must not be empty." );

		doc = getDoc();
		
		style = doc.getStyle( styleName );
		if( style == null )
			throw new NullPointerException( "A style with name '"+styleName+"' has never been defined." );
		
		start = t.getStartIndex();		
		if( start < 0 )
			return;
		
		len = t.getStopIndex()-start+1;
		if( len <= 0 )
			return;

		doc.setCharacterAttributes(
			start,
			len,
			style,
			false );
	}
	
	private void mark( ParserRuleContext ctx, String styleName ) {
		mark( ctx, styleName, false );
	}
	
	private void mark( ParserRuleContext ctx, String styleName, boolean supersede ) {
		
		Style style;
		int start, len;
		StyledDocument doc;
		
		if( ctx == null )
			throw new NullPointerException( "Context must not be null." );
		
		if( styleName == null )
			throw new NullPointerException( "Style name must not be null." );
		
		if( styleName.isEmpty() )
			throw new RuntimeException( "Style name must not be empty." );
		
		if( ctx.stop == null )
			return;
		
		if( ctx.start == null )
			return;
		
		doc = getDoc();
		
		style = doc.getStyle( styleName );
		if( style == null )
			throw new NullPointerException( "A style with name '"+styleName+"' has never been defined." );
		
		start = getStart( ctx );
		if( start < 0 )
			return;
		
		len = getLen( ctx );
		if( len <= 0 )
			return;
		
		doc.setCharacterAttributes(
			start,
			len,
			style,
			supersede );
	}
	
	private static int getStart( ParserRuleContext ctx ) {
		
		if( ctx == null )
			throw new NullPointerException( "Context must not be null." );
		
		if( ctx.start == null )
			throw new NullPointerException( "Start token must not be null." );
		
		return ctx.start.getStartIndex();
	}
	
	private static int getLen( ParserRuleContext ctx ) {
		
		if( ctx == null )
			throw new NullPointerException( "Context must not be null." );
		
		if( ctx.stop == null )
			throw new NullPointerException( "Stop token must not be null." );
		
		return ctx.stop.getStopIndex()-getStart( ctx )+1;
	}

}
