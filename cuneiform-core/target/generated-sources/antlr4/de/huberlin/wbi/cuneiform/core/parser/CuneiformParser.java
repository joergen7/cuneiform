// Generated from de/huberlin/wbi/cuneiform/core/parser/Cuneiform.g4 by ANTLR 4.2
package de.huberlin.wbi.cuneiform.core.parser;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class CuneiformParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		APPLY=1, COLON=2, COMB=3, COMBR=4, CURRY=5, DEFTASK=6, EQUAL=7, ELSE=8, 
		FROMSTACK=9, IF=10, IN=11, IMPORT=12, INCLUDE=13, LAMBDA=14, LBRACE=15, 
		LPAREN=16, LSQUAREBR=17, LTAG=18, NIL=19, PERM=20, RBRACE=21, RPAREN=22, 
		RSQUAREBR=23, RTAG=24, SEMICOLON=25, TILDE=26, THEN=27, TOSTACK=28, VAR=29, 
		INT=30, BODY=31, OPENBODY=32, STRING=33, COMMENT=34, ID=35, WS=36;
	public static final String[] tokenNames = {
		"<INVALID>", "'apply'", "':'", "'comb'", "'combr'", "'curry'", "'deftask'", 
		"'='", "'else'", "FROMSTACK", "'if'", "'in'", "'import'", "'include'", 
		"'\\'", "'{'", "'('", "'['", "'<'", "'nil'", "'perm'", "'}'", "')'", "']'", 
		"'>'", "';'", "'~'", "'then'", "TOSTACK", "'var'", "INT", "BODY", "OPENBODY", 
		"STRING", "COMMENT", "ID", "WS"
	};
	public static final int
		RULE_script = 0, RULE_stat = 1, RULE_instat = 2, RULE_importFile = 3, 
		RULE_assign = 4, RULE_defTask = 5, RULE_prototype = 6, RULE_name = 7, 
		RULE_param = 8, RULE_draw = 9, RULE_output = 10, RULE_reduceVar = 11, 
		RULE_correlParam = 12, RULE_expr = 13, RULE_danglingExpr = 14, RULE_singleExpr = 15, 
		RULE_channel = 16, RULE_block = 17, RULE_paramBind = 18, RULE_target = 19, 
		RULE_foreignBody = 20;
	public static final String[] ruleNames = {
		"script", "stat", "instat", "importFile", "assign", "defTask", "prototype", 
		"name", "param", "draw", "output", "reduceVar", "correlParam", "expr", 
		"danglingExpr", "singleExpr", "channel", "block", "paramBind", "target", 
		"foreignBody"
	};

	@Override
	public String getGrammarFileName() { return "Cuneiform.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public CuneiformParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class ScriptContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(CuneiformParser.EOF, 0); }
		public StatContext stat(int i) {
			return getRuleContext(StatContext.class,i);
		}
		public List<StatContext> stat() {
			return getRuleContexts(StatContext.class);
		}
		public ScriptContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_script; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterScript(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitScript(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitScript(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ScriptContext script() throws RecognitionException {
		ScriptContext _localctx = new ScriptContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_script);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(45);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << APPLY) | (1L << CURRY) | (1L << DEFTASK) | (1L << FROMSTACK) | (1L << IF) | (1L << IMPORT) | (1L << INCLUDE) | (1L << LAMBDA) | (1L << LSQUAREBR) | (1L << NIL) | (1L << INT) | (1L << STRING) | (1L << ID))) != 0)) {
				{
				{
				setState(42); stat();
				}
				}
				setState(47);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(48); match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StatContext extends ParserRuleContext {
		public ImportFileContext importFile() {
			return getRuleContext(ImportFileContext.class,0);
		}
		public TargetContext target() {
			return getRuleContext(TargetContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public InstatContext instat() {
			return getRuleContext(InstatContext.class,0);
		}
		public StatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stat; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterStat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitStat(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitStat(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StatContext stat() throws RecognitionException {
		StatContext _localctx = new StatContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_stat);
		try {
			setState(56);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(50); target();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(51); importFile();
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(52); instat();
				}
				break;

			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(53); expr();
				 notifyErrorListeners( "Dangling expression. Expecting ';' or '-+'." ); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class InstatContext extends ParserRuleContext {
		public DefTaskContext defTask() {
			return getRuleContext(DefTaskContext.class,0);
		}
		public AssignContext assign() {
			return getRuleContext(AssignContext.class,0);
		}
		public DanglingExprContext danglingExpr() {
			return getRuleContext(DanglingExprContext.class,0);
		}
		public InstatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_instat; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterInstat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitInstat(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitInstat(this);
			else return visitor.visitChildren(this);
		}
	}

	public final InstatContext instat() throws RecognitionException {
		InstatContext _localctx = new InstatContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_instat);
		try {
			setState(61);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(58); assign();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(59); defTask();
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(60); danglingExpr();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ImportFileContext extends ParserRuleContext {
		public TerminalNode SEMICOLON() { return getToken(CuneiformParser.SEMICOLON, 0); }
		public TerminalNode IMPORT() { return getToken(CuneiformParser.IMPORT, 0); }
		public TerminalNode STRING() { return getToken(CuneiformParser.STRING, 0); }
		public TerminalNode INCLUDE() { return getToken(CuneiformParser.INCLUDE, 0); }
		public ImportFileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_importFile; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterImportFile(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitImportFile(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitImportFile(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ImportFileContext importFile() throws RecognitionException {
		ImportFileContext _localctx = new ImportFileContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_importFile);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(63);
			_la = _input.LA(1);
			if ( !(_la==IMPORT || _la==INCLUDE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(64); match(STRING);
			setState(65); match(SEMICOLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AssignContext extends ParserRuleContext {
		public TerminalNode EQUAL() { return getToken(CuneiformParser.EQUAL, 0); }
		public TerminalNode SEMICOLON() { return getToken(CuneiformParser.SEMICOLON, 0); }
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public AssignContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assign; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterAssign(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitAssign(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitAssign(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AssignContext assign() throws RecognitionException {
		AssignContext _localctx = new AssignContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_assign);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(68); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(67); name();
				}
				}
				setState(70); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==ID );
			setState(72); match(EQUAL);
			setState(73); expr();
			setState(74); match(SEMICOLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DefTaskContext extends ParserRuleContext {
		public DefTaskContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_defTask; }
	 
		public DefTaskContext() { }
		public void copyFrom(DefTaskContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class DefTaskErr3Context extends DefTaskContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode RBRACE() { return getToken(CuneiformParser.RBRACE, 0); }
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public DefTaskErr3Context(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDefTaskErr3(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDefTaskErr3(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDefTaskErr3(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NativeDefTaskContext extends DefTaskContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode RBRACE() { return getToken(CuneiformParser.RBRACE, 0); }
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public List<InstatContext> instat() {
			return getRuleContexts(InstatContext.class);
		}
		public InstatContext instat(int i) {
			return getRuleContext(InstatContext.class,i);
		}
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public NativeDefTaskContext(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNativeDefTask(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNativeDefTask(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNativeDefTask(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FnPrototypeErr3Context extends DefTaskContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode RPAREN(int i) {
			return getToken(CuneiformParser.RPAREN, i);
		}
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public OutputContext output(int i) {
			return getRuleContext(OutputContext.class,i);
		}
		public List<TerminalNode> RPAREN() { return getTokens(CuneiformParser.RPAREN); }
		public List<OutputContext> output() {
			return getRuleContexts(OutputContext.class);
		}
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public FnPrototypeErr3Context(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterFnPrototypeErr3(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitFnPrototypeErr3(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitFnPrototypeErr3(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class DefTaskErr1Context extends DefTaskContext {
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public DefTaskErr1Context(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDefTaskErr1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDefTaskErr1(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDefTaskErr1(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FnPrototypeErr2Context extends DefTaskContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public OutputContext output(int i) {
			return getRuleContext(OutputContext.class,i);
		}
		public List<OutputContext> output() {
			return getRuleContexts(OutputContext.class);
		}
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public FnPrototypeErr2Context(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterFnPrototypeErr2(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitFnPrototypeErr2(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitFnPrototypeErr2(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class DefTaskErr2Context extends DefTaskContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public DefTaskErr2Context(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDefTaskErr2(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDefTaskErr2(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDefTaskErr2(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FnPrototypeErr1Context extends DefTaskContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public FnPrototypeErr1Context(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterFnPrototypeErr1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitFnPrototypeErr1(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitFnPrototypeErr1(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ForeignDefTaskContext extends DefTaskContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public ForeignBodyContext foreignBody() {
			return getRuleContext(ForeignBodyContext.class,0);
		}
		public TerminalNode DEFTASK() { return getToken(CuneiformParser.DEFTASK, 0); }
		public ForeignDefTaskContext(DefTaskContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterForeignDefTask(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitForeignDefTask(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitForeignDefTask(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DefTaskContext defTask() throws RecognitionException {
		DefTaskContext _localctx = new DefTaskContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_defTask);
		int _la;
		try {
			int _alt;
			setState(151);
			switch ( getInterpreter().adaptivePredict(_input,10,_ctx) ) {
			case 1:
				_localctx = new NativeDefTaskContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(76); match(DEFTASK);
				setState(77); match(ID);
				setState(78); prototype();
				setState(79); match(LBRACE);
				setState(81); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(80); instat();
					}
					}
					setState(83); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << APPLY) | (1L << CURRY) | (1L << DEFTASK) | (1L << FROMSTACK) | (1L << IF) | (1L << LAMBDA) | (1L << LSQUAREBR) | (1L << NIL) | (1L << INT) | (1L << STRING) | (1L << ID))) != 0) );
				setState(85); match(RBRACE);
				}
				break;

			case 2:
				_localctx = new ForeignDefTaskContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(87); match(DEFTASK);
				setState(88); match(ID);
				setState(89); prototype();
				setState(90); foreignBody();
				}
				break;

			case 3:
				_localctx = new DefTaskErr1Context(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(92); match(DEFTASK);
				 notifyErrorListeners( "Incomplete Task definition. Task name expected." ); 
				}
				break;

			case 4:
				_localctx = new DefTaskErr2Context(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(94); match(DEFTASK);
				setState(95); match(ID);
				setState(96); prototype();
				setState(97); match(LBRACE);
				 notifyErrorListeners( "Missing '}'." ); 
				}
				break;

			case 5:
				_localctx = new DefTaskErr3Context(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(100); match(DEFTASK);
				setState(101); match(ID);
				setState(102); prototype();
				setState(103); match(LBRACE);
				setState(104); match(RBRACE);
				 notifyErrorListeners( "Empty native task block." ); 
				}
				break;

			case 6:
				_localctx = new FnPrototypeErr1Context(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(107); match(DEFTASK);
				setState(108); match(ID);
				setState(109); match(LPAREN);
				 notifyErrorListeners( "Incomplete task prototype. Expecting at least one output variable declaration." ); 
				}
				break;

			case 7:
				_localctx = new FnPrototypeErr2Context(_localctx);
				enterOuterAlt(_localctx, 7);
				{
				setState(111); match(DEFTASK);
				setState(112); match(ID);
				setState(113); match(LPAREN);
				setState(115); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(114); output();
					}
					}
					setState(117); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==LTAG || _la==ID );
				setState(119); match(COLON);
				setState(123);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
				while ( _alt!=2 && _alt!=-1 ) {
					if ( _alt==1 ) {
						{
						{
						setState(120); param();
						}
						} 
					}
					setState(125);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
				}
				 notifyErrorListeners( "Incomplete task prototype. Expecting input parameter declaration or ')'." ); 
				}
				break;

			case 8:
				_localctx = new FnPrototypeErr3Context(_localctx);
				enterOuterAlt(_localctx, 8);
				{
				setState(128); match(DEFTASK);
				setState(129); match(ID);
				setState(130); match(LPAREN);
				setState(132); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(131); output();
					}
					}
					setState(134); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==LTAG || _la==ID );
				setState(136); match(COLON);
				setState(140);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << LBRACE) | (1L << LSQUAREBR) | (1L << LTAG) | (1L << ID))) != 0)) {
					{
					{
					setState(137); param();
					}
					}
					setState(142);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(143); match(RPAREN);
				setState(145); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(144); match(RPAREN);
					}
					}
					setState(147); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==RPAREN );
				 notifyErrorListeners( "Too many ')'." ); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PrototypeContext extends ParserRuleContext {
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public OutputContext output(int i) {
			return getRuleContext(OutputContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(CuneiformParser.RPAREN, 0); }
		public List<OutputContext> output() {
			return getRuleContexts(OutputContext.class);
		}
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public PrototypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_prototype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterPrototype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitPrototype(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitPrototype(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PrototypeContext prototype() throws RecognitionException {
		PrototypeContext _localctx = new PrototypeContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_prototype);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(153); match(LPAREN);
			setState(155); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(154); output();
				}
				}
				setState(157); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==LTAG || _la==ID );
			setState(159); match(COLON);
			setState(163);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << LBRACE) | (1L << LSQUAREBR) | (1L << LTAG) | (1L << ID))) != 0)) {
				{
				{
				setState(160); param();
				}
				}
				setState(165);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(166); match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class NameContext extends ParserRuleContext {
		public NameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_name; }
	 
		public NameContext() { }
		public void copyFrom(NameContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class NameErr2Context extends NameContext {
		public List<TerminalNode> ID() { return getTokens(CuneiformParser.ID); }
		public TerminalNode RPAREN(int i) {
			return getToken(CuneiformParser.RPAREN, i);
		}
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode ID(int i) {
			return getToken(CuneiformParser.ID, i);
		}
		public List<TerminalNode> RPAREN() { return getTokens(CuneiformParser.RPAREN); }
		public NameErr2Context(NameContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNameErr2(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNameErr2(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNameErr2(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NameDataTypeContext extends NameContext {
		public List<TerminalNode> ID() { return getTokens(CuneiformParser.ID); }
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode ID(int i) {
			return getToken(CuneiformParser.ID, i);
		}
		public TerminalNode RPAREN() { return getToken(CuneiformParser.RPAREN, 0); }
		public NameDataTypeContext(NameContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNameDataType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNameDataType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNameDataType(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NamePlainFnTypeContext extends NameContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public TerminalNode RPAREN() { return getToken(CuneiformParser.RPAREN, 0); }
		public NamePlainFnTypeContext(NameContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNamePlainFnType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNamePlainFnType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNamePlainFnType(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NameDeepFnTypeContext extends NameContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public NameDeepFnTypeContext(NameContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNameDeepFnType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNameDeepFnType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNameDeepFnType(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NameInferredTypeContext extends NameContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public NameInferredTypeContext(NameContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNameInferredType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNameInferredType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNameInferredType(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NameErr1Context extends NameContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public NameErr1Context(NameContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNameErr1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNameErr1(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNameErr1(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NameContext name() throws RecognitionException {
		NameContext _localctx = new NameContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_name);
		try {
			int _alt;
			setState(195);
			switch ( getInterpreter().adaptivePredict(_input,15,_ctx) ) {
			case 1:
				_localctx = new NameInferredTypeContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(168); match(ID);
				}
				break;

			case 2:
				_localctx = new NameDataTypeContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(169); match(ID);
				setState(170); match(LPAREN);
				setState(171); match(ID);
				setState(172); match(RPAREN);
				}
				break;

			case 3:
				_localctx = new NamePlainFnTypeContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(173); match(ID);
				setState(174); match(LPAREN);
				setState(175); match(COLON);
				setState(176); match(RPAREN);
				}
				break;

			case 4:
				_localctx = new NameDeepFnTypeContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(177); match(ID);
				setState(178); prototype();
				}
				break;

			case 5:
				_localctx = new NameErr1Context(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(179); match(ID);
				setState(180); match(LPAREN);
				setState(182);
				switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
				case 1:
					{
					setState(181); match(COLON);
					}
					break;
				}
				 notifyErrorListeners( "Missing ')'." ); 
				}
				break;

			case 6:
				_localctx = new NameErr2Context(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(185); match(ID);
				setState(186); match(LPAREN);
				setState(187); match(ID);
				setState(188); match(RPAREN);
				setState(190); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(189); match(RPAREN);
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(192); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
				} while ( _alt!=2 && _alt!=-1 );
				 notifyErrorListeners( "Too many ')'." ); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamContext extends ParserRuleContext {
		public CorrelParamContext correlParam() {
			return getRuleContext(CorrelParamContext.class,0);
		}
		public DrawContext draw() {
			return getRuleContext(DrawContext.class,0);
		}
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public ReduceVarContext reduceVar() {
			return getRuleContext(ReduceVarContext.class,0);
		}
		public ParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_param; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitParam(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitParam(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ParamContext param() throws RecognitionException {
		ParamContext _localctx = new ParamContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_param);
		try {
			setState(201);
			switch (_input.LA(1)) {
			case ID:
				enterOuterAlt(_localctx, 1);
				{
				setState(197); name();
				}
				break;
			case LTAG:
				enterOuterAlt(_localctx, 2);
				{
				setState(198); reduceVar();
				}
				break;
			case LSQUAREBR:
				enterOuterAlt(_localctx, 3);
				{
				setState(199); correlParam();
				}
				break;
			case LBRACE:
				enterOuterAlt(_localctx, 4);
				{
				setState(200); draw();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DrawContext extends ParserRuleContext {
		public DrawContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_draw; }
	 
		public DrawContext() { }
		public void copyFrom(DrawContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class DrawCombrContext extends DrawContext {
		public TerminalNode RBRACE() { return getToken(CuneiformParser.RBRACE, 0); }
		public CorrelParamContext correlParam() {
			return getRuleContext(CorrelParamContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public TerminalNode COMBR() { return getToken(CuneiformParser.COMBR, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode INT() { return getToken(CuneiformParser.INT, 0); }
		public DrawCombrContext(DrawContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDrawCombr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDrawCombr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDrawCombr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class DrawPermContext extends DrawContext {
		public TerminalNode PERM() { return getToken(CuneiformParser.PERM, 0); }
		public TerminalNode RBRACE() { return getToken(CuneiformParser.RBRACE, 0); }
		public CorrelParamContext correlParam() {
			return getRuleContext(CorrelParamContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode INT() { return getToken(CuneiformParser.INT, 0); }
		public DrawPermContext(DrawContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDrawPerm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDrawPerm(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDrawPerm(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class DrawCombContext extends DrawContext {
		public TerminalNode RBRACE() { return getToken(CuneiformParser.RBRACE, 0); }
		public CorrelParamContext correlParam() {
			return getRuleContext(CorrelParamContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public TerminalNode COMB() { return getToken(CuneiformParser.COMB, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode INT() { return getToken(CuneiformParser.INT, 0); }
		public DrawCombContext(DrawContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDrawComb(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDrawComb(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDrawComb(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class DrawVarContext extends DrawContext {
		public TerminalNode RBRACE() { return getToken(CuneiformParser.RBRACE, 0); }
		public TerminalNode VAR() { return getToken(CuneiformParser.VAR, 0); }
		public CorrelParamContext correlParam() {
			return getRuleContext(CorrelParamContext.class,0);
		}
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode INT() { return getToken(CuneiformParser.INT, 0); }
		public DrawVarContext(DrawContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDrawVar(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDrawVar(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDrawVar(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DrawContext draw() throws RecognitionException {
		DrawContext _localctx = new DrawContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_draw);
		try {
			setState(243);
			switch ( getInterpreter().adaptivePredict(_input,21,_ctx) ) {
			case 1:
				_localctx = new DrawCombContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(203); match(LBRACE);
				setState(204); match(COMB);
				setState(205); match(INT);
				setState(206); match(COLON);
				setState(209);
				switch (_input.LA(1)) {
				case ID:
					{
					setState(207); name();
					}
					break;
				case LSQUAREBR:
					{
					setState(208); correlParam();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(211); match(RBRACE);
				}
				break;

			case 2:
				_localctx = new DrawCombrContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(213); match(LBRACE);
				setState(214); match(COMBR);
				setState(215); match(INT);
				setState(216); match(COLON);
				setState(219);
				switch (_input.LA(1)) {
				case ID:
					{
					setState(217); name();
					}
					break;
				case LSQUAREBR:
					{
					setState(218); correlParam();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(221); match(RBRACE);
				}
				break;

			case 3:
				_localctx = new DrawVarContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(223); match(LBRACE);
				setState(224); match(VAR);
				setState(225); match(INT);
				setState(226); match(COLON);
				setState(229);
				switch (_input.LA(1)) {
				case ID:
					{
					setState(227); name();
					}
					break;
				case LSQUAREBR:
					{
					setState(228); correlParam();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(231); match(RBRACE);
				}
				break;

			case 4:
				_localctx = new DrawPermContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(233); match(LBRACE);
				setState(234); match(PERM);
				setState(235); match(INT);
				setState(236); match(COLON);
				setState(239);
				switch (_input.LA(1)) {
				case ID:
					{
					setState(237); name();
					}
					break;
				case LSQUAREBR:
					{
					setState(238); correlParam();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(241); match(RBRACE);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OutputContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public ReduceVarContext reduceVar() {
			return getRuleContext(ReduceVarContext.class,0);
		}
		public OutputContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_output; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterOutput(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitOutput(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitOutput(this);
			else return visitor.visitChildren(this);
		}
	}

	public final OutputContext output() throws RecognitionException {
		OutputContext _localctx = new OutputContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_output);
		try {
			setState(247);
			switch (_input.LA(1)) {
			case ID:
				enterOuterAlt(_localctx, 1);
				{
				setState(245); name();
				}
				break;
			case LTAG:
				enterOuterAlt(_localctx, 2);
				{
				setState(246); reduceVar();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ReduceVarContext extends ParserRuleContext {
		public TerminalNode LTAG() { return getToken(CuneiformParser.LTAG, 0); }
		public TerminalNode RTAG() { return getToken(CuneiformParser.RTAG, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public ReduceVarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_reduceVar; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterReduceVar(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitReduceVar(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitReduceVar(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ReduceVarContext reduceVar() throws RecognitionException {
		ReduceVarContext _localctx = new ReduceVarContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_reduceVar);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(249); match(LTAG);
			setState(250); name();
			setState(251); match(RTAG);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CorrelParamContext extends ParserRuleContext {
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public TerminalNode RSQUAREBR() { return getToken(CuneiformParser.RSQUAREBR, 0); }
		public TerminalNode LSQUAREBR() { return getToken(CuneiformParser.LSQUAREBR, 0); }
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public CorrelParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_correlParam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterCorrelParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitCorrelParam(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitCorrelParam(this);
			else return visitor.visitChildren(this);
		}
	}

	public final CorrelParamContext correlParam() throws RecognitionException {
		CorrelParamContext _localctx = new CorrelParamContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_correlParam);
		int _la;
		try {
			setState(270);
			switch ( getInterpreter().adaptivePredict(_input,24,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(253); match(LSQUAREBR);
				setState(254); name();
				setState(256); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(255); name();
					}
					}
					setState(258); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==ID );
				setState(260); match(RSQUAREBR);
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(262); match(LSQUAREBR);
				setState(263); name();
				setState(264); match(RSQUAREBR);
				 notifyErrorListeners( "Correlated parameter list must have at least two entries." ); 
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(267); match(LSQUAREBR);
				setState(268); match(RSQUAREBR);
				 notifyErrorListeners( "Empty correlated parameter list." ); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprContext extends ParserRuleContext {
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	 
		public ExprContext() { }
		public void copyFrom(ExprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class CompoundExprContext extends ExprContext {
		public List<SingleExprContext> singleExpr() {
			return getRuleContexts(SingleExprContext.class);
		}
		public SingleExprContext singleExpr(int i) {
			return getRuleContext(SingleExprContext.class,i);
		}
		public CompoundExprContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterCompoundExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitCompoundExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitCompoundExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NilExprContext extends ExprContext {
		public TerminalNode NIL() { return getToken(CuneiformParser.NIL, 0); }
		public NilExprContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNilExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNilExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNilExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_expr);
		try {
			int _alt;
			setState(278);
			switch (_input.LA(1)) {
			case NIL:
				_localctx = new NilExprContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(272); match(NIL);
				}
				break;
			case APPLY:
			case CURRY:
			case FROMSTACK:
			case IF:
			case LAMBDA:
			case LSQUAREBR:
			case INT:
			case STRING:
			case ID:
				_localctx = new CompoundExprContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(274); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,25,_ctx);
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(273); singleExpr();
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(276); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,25,_ctx);
				} while ( _alt!=2 && _alt!=-1 );
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DanglingExprContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode TOSTACK() { return getToken(CuneiformParser.TOSTACK, 0); }
		public DanglingExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_danglingExpr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterDanglingExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitDanglingExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitDanglingExpr(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DanglingExprContext danglingExpr() throws RecognitionException {
		DanglingExprContext _localctx = new DanglingExprContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_danglingExpr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(280); expr();
			setState(281); match(TOSTACK);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SingleExprContext extends ParserRuleContext {
		public SingleExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_singleExpr; }
	 
		public SingleExprContext() { }
		public void copyFrom(SingleExprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class StringExprContext extends SingleExprContext {
		public TerminalNode STRING() { return getToken(CuneiformParser.STRING, 0); }
		public StringExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterStringExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitStringExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitStringExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class IdExprContext extends SingleExprContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public IdExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterIdExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitIdExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitIdExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class CondExprContext extends SingleExprContext {
		public TerminalNode ELSE() { return getToken(CuneiformParser.ELSE, 0); }
		public TerminalNode IF() { return getToken(CuneiformParser.IF, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public BlockContext block(int i) {
			return getRuleContext(BlockContext.class,i);
		}
		public TerminalNode THEN() { return getToken(CuneiformParser.THEN, 0); }
		public List<BlockContext> block() {
			return getRuleContexts(BlockContext.class);
		}
		public ChannelContext channel() {
			return getRuleContext(ChannelContext.class,0);
		}
		public CondExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterCondExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitCondExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitCondExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ForeignLambdaExprContext extends SingleExprContext {
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public TerminalNode LAMBDA() { return getToken(CuneiformParser.LAMBDA, 0); }
		public ForeignBodyContext foreignBody() {
			return getRuleContext(ForeignBodyContext.class,0);
		}
		public ForeignLambdaExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterForeignLambdaExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitForeignLambdaExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitForeignLambdaExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ApplyExprContext extends SingleExprContext {
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public ParamBindContext paramBind(int i) {
			return getRuleContext(ParamBindContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(CuneiformParser.RPAREN, 0); }
		public List<ParamBindContext> paramBind() {
			return getRuleContexts(ParamBindContext.class);
		}
		public TerminalNode TILDE() { return getToken(CuneiformParser.TILDE, 0); }
		public TerminalNode APPLY() { return getToken(CuneiformParser.APPLY, 0); }
		public ChannelContext channel() {
			return getRuleContext(ChannelContext.class,0);
		}
		public ApplyExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterApplyExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitApplyExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitApplyExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FromStackExprContext extends SingleExprContext {
		public TerminalNode FROMSTACK() { return getToken(CuneiformParser.FROMSTACK, 0); }
		public FromStackExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterFromStackExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitFromStackExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitFromStackExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class SingleExprErr1Context extends SingleExprContext {
		public TerminalNode APPLY() { return getToken(CuneiformParser.APPLY, 0); }
		public SingleExprErr1Context(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterSingleExprErr1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitSingleExprErr1(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitSingleExprErr1(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ParamBindErr1Context extends SingleExprContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public TerminalNode APPLY() { return getToken(CuneiformParser.APPLY, 0); }
		public ParamBindErr1Context(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterParamBindErr1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitParamBindErr1(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitParamBindErr1(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class SingleExprErr3Context extends SingleExprContext {
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public ParamBindContext paramBind(int i) {
			return getRuleContext(ParamBindContext.class,i);
		}
		public List<ParamBindContext> paramBind() {
			return getRuleContexts(ParamBindContext.class);
		}
		public TerminalNode APPLY() { return getToken(CuneiformParser.APPLY, 0); }
		public SingleExprErr3Context(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterSingleExprErr3(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitSingleExprErr3(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitSingleExprErr3(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class CurryExprContext extends SingleExprContext {
		public TerminalNode CURRY() { return getToken(CuneiformParser.CURRY, 0); }
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public ParamBindContext paramBind(int i) {
			return getRuleContext(ParamBindContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(CuneiformParser.RPAREN, 0); }
		public List<ParamBindContext> paramBind() {
			return getRuleContexts(ParamBindContext.class);
		}
		public CurryExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterCurryExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitCurryExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitCurryExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class SingleExprErr2Context extends SingleExprContext {
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public TerminalNode APPLY() { return getToken(CuneiformParser.APPLY, 0); }
		public SingleExprErr2Context(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterSingleExprErr2(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitSingleExprErr2(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitSingleExprErr2(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class CallExprContext extends SingleExprContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode LPAREN() { return getToken(CuneiformParser.LPAREN, 0); }
		public ParamBindContext paramBind(int i) {
			return getRuleContext(ParamBindContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(CuneiformParser.RPAREN, 0); }
		public List<ParamBindContext> paramBind() {
			return getRuleContexts(ParamBindContext.class);
		}
		public TerminalNode TILDE() { return getToken(CuneiformParser.TILDE, 0); }
		public ChannelContext channel() {
			return getRuleContext(ChannelContext.class,0);
		}
		public CallExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterCallExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitCallExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitCallExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class NativeLambdaExprContext extends SingleExprContext {
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public TerminalNode LAMBDA() { return getToken(CuneiformParser.LAMBDA, 0); }
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public NativeLambdaExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterNativeLambdaExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitNativeLambdaExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitNativeLambdaExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ForeignFnBodyErr1Context extends SingleExprContext {
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public TerminalNode LAMBDA() { return getToken(CuneiformParser.LAMBDA, 0); }
		public TerminalNode IN() { return getToken(CuneiformParser.IN, 0); }
		public ForeignFnBodyErr1Context(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterForeignFnBodyErr1(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitForeignFnBodyErr1(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitForeignFnBodyErr1(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class IntExprContext extends SingleExprContext {
		public TerminalNode INT() { return getToken(CuneiformParser.INT, 0); }
		public IntExprContext(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterIntExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitIntExpr(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitIntExpr(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ForeignFnBodyErr2Context extends SingleExprContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public PrototypeContext prototype() {
			return getRuleContext(PrototypeContext.class,0);
		}
		public TerminalNode LAMBDA() { return getToken(CuneiformParser.LAMBDA, 0); }
		public TerminalNode OPENBODY() { return getToken(CuneiformParser.OPENBODY, 0); }
		public TerminalNode IN() { return getToken(CuneiformParser.IN, 0); }
		public ForeignFnBodyErr2Context(SingleExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterForeignFnBodyErr2(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitForeignFnBodyErr2(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitForeignFnBodyErr2(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SingleExprContext singleExpr() throws RecognitionException {
		SingleExprContext _localctx = new SingleExprContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_singleExpr);
		int _la;
		try {
			int _alt;
			setState(376);
			switch ( getInterpreter().adaptivePredict(_input,36,_ctx) ) {
			case 1:
				_localctx = new IdExprContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(283); match(ID);
				}
				break;

			case 2:
				_localctx = new IntExprContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(284); match(INT);
				}
				break;

			case 3:
				_localctx = new StringExprContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(285); match(STRING);
				}
				break;

			case 4:
				_localctx = new FromStackExprContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(286); match(FROMSTACK);
				}
				break;

			case 5:
				_localctx = new CondExprContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(288);
				_la = _input.LA(1);
				if (_la==LSQUAREBR) {
					{
					setState(287); channel();
					}
				}

				setState(290); match(IF);
				setState(291); prototype();
				setState(292); expr();
				setState(293); match(THEN);
				setState(294); block();
				setState(295); match(ELSE);
				setState(296); block();
				}
				break;

			case 6:
				_localctx = new ApplyExprContext(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(299);
				_la = _input.LA(1);
				if (_la==LSQUAREBR) {
					{
					setState(298); channel();
					}
				}

				setState(301); match(APPLY);
				setState(302); match(LPAREN);
				setState(304); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(303); paramBind();
					}
					}
					setState(306); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==ID );
				setState(309);
				_la = _input.LA(1);
				if (_la==TILDE) {
					{
					setState(308); match(TILDE);
					}
				}

				setState(311); match(RPAREN);
				}
				break;

			case 7:
				_localctx = new CallExprContext(_localctx);
				enterOuterAlt(_localctx, 7);
				{
				setState(314);
				_la = _input.LA(1);
				if (_la==LSQUAREBR) {
					{
					setState(313); channel();
					}
				}

				setState(316); match(ID);
				setState(317); match(LPAREN);
				setState(321);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==ID) {
					{
					{
					setState(318); paramBind();
					}
					}
					setState(323);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(325);
				_la = _input.LA(1);
				if (_la==TILDE) {
					{
					setState(324); match(TILDE);
					}
				}

				setState(327); match(RPAREN);
				}
				break;

			case 8:
				_localctx = new CurryExprContext(_localctx);
				enterOuterAlt(_localctx, 8);
				{
				setState(328); match(CURRY);
				setState(329); match(LPAREN);
				setState(331); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(330); paramBind();
					}
					}
					setState(333); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==ID );
				setState(335); match(RPAREN);
				}
				break;

			case 9:
				_localctx = new NativeLambdaExprContext(_localctx);
				enterOuterAlt(_localctx, 9);
				{
				setState(337); match(LAMBDA);
				setState(338); prototype();
				setState(339); block();
				}
				break;

			case 10:
				_localctx = new ForeignLambdaExprContext(_localctx);
				enterOuterAlt(_localctx, 10);
				{
				setState(341); match(LAMBDA);
				setState(342); prototype();
				setState(343); foreignBody();
				}
				break;

			case 11:
				_localctx = new SingleExprErr1Context(_localctx);
				enterOuterAlt(_localctx, 11);
				{
				setState(345); match(APPLY);
				 notifyErrorListeners( "Incomplete task application. Missing '('." ); 
				}
				break;

			case 12:
				_localctx = new SingleExprErr2Context(_localctx);
				enterOuterAlt(_localctx, 12);
				{
				setState(347); match(APPLY);
				setState(348); match(LPAREN);
				 notifyErrorListeners( "Incomplete task application. Missing Parameter bindings, e.g. 'param: value'." ); 
				}
				break;

			case 13:
				_localctx = new SingleExprErr3Context(_localctx);
				enterOuterAlt(_localctx, 13);
				{
				setState(350); match(APPLY);
				setState(351); match(LPAREN);
				setState(353); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,35,_ctx);
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(352); paramBind();
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(355); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,35,_ctx);
				} while ( _alt!=2 && _alt!=-1 );
				 notifyErrorListeners( "Incomplete task application. Missing ')'." ); 
				}
				break;

			case 14:
				_localctx = new ParamBindErr1Context(_localctx);
				enterOuterAlt(_localctx, 14);
				{
				setState(359); match(APPLY);
				setState(360); match(LPAREN);
				setState(361); match(ID);
				setState(362); match(COLON);
				 notifyErrorListeners( "Incomplete Parameter binding. Missing value." ); 
				}
				break;

			case 15:
				_localctx = new ForeignFnBodyErr1Context(_localctx);
				enterOuterAlt(_localctx, 15);
				{
				setState(364); match(LAMBDA);
				setState(365); prototype();
				setState(366); match(IN);
				 notifyErrorListeners( "In foreign task definition: Expecting lang id (e.g. 'bash' or 'python')." ); 
				}
				break;

			case 16:
				_localctx = new ForeignFnBodyErr2Context(_localctx);
				enterOuterAlt(_localctx, 16);
				{
				setState(369); match(LAMBDA);
				setState(370); prototype();
				setState(371); match(IN);
				setState(372); match(ID);
				setState(373); match(OPENBODY);
				 notifyErrorListeners( "In foreign task definition: Missing '}*'." ); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ChannelContext extends ParserRuleContext {
		public TerminalNode RSQUAREBR() { return getToken(CuneiformParser.RSQUAREBR, 0); }
		public TerminalNode LSQUAREBR() { return getToken(CuneiformParser.LSQUAREBR, 0); }
		public TerminalNode INT() { return getToken(CuneiformParser.INT, 0); }
		public ChannelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_channel; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterChannel(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitChannel(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitChannel(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ChannelContext channel() throws RecognitionException {
		ChannelContext _localctx = new ChannelContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_channel);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(378); match(LSQUAREBR);
			setState(379); match(INT);
			setState(380); match(RSQUAREBR);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BlockContext extends ParserRuleContext {
		public TerminalNode RBRACE() { return getToken(CuneiformParser.RBRACE, 0); }
		public TerminalNode LBRACE() { return getToken(CuneiformParser.LBRACE, 0); }
		public List<InstatContext> instat() {
			return getRuleContexts(InstatContext.class);
		}
		public InstatContext instat(int i) {
			return getRuleContext(InstatContext.class,i);
		}
		public BlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_block; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BlockContext block() throws RecognitionException {
		BlockContext _localctx = new BlockContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_block);
		int _la;
		try {
			setState(393);
			switch ( getInterpreter().adaptivePredict(_input,38,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(382); match(LBRACE);
				setState(384); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(383); instat();
					}
					}
					setState(386); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << APPLY) | (1L << CURRY) | (1L << DEFTASK) | (1L << FROMSTACK) | (1L << IF) | (1L << LAMBDA) | (1L << LSQUAREBR) | (1L << NIL) | (1L << INT) | (1L << STRING) | (1L << ID))) != 0) );
				setState(388); match(RBRACE);
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(390); match(LBRACE);
				setState(391); match(RBRACE);
				 notifyErrorListeners( "Empty block. Expecting target, assignment, or task definition." ); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamBindContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode COLON() { return getToken(CuneiformParser.COLON, 0); }
		public ParamBindContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramBind; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterParamBind(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitParamBind(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitParamBind(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ParamBindContext paramBind() throws RecognitionException {
		ParamBindContext _localctx = new ParamBindContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_paramBind);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(395); match(ID);
			setState(396); match(COLON);
			setState(397); expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TargetContext extends ParserRuleContext {
		public TerminalNode SEMICOLON() { return getToken(CuneiformParser.SEMICOLON, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TargetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_target; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterTarget(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitTarget(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitTarget(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TargetContext target() throws RecognitionException {
		TargetContext _localctx = new TargetContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_target);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(399); expr();
			setState(400); match(SEMICOLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ForeignBodyContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(CuneiformParser.ID, 0); }
		public TerminalNode BODY() { return getToken(CuneiformParser.BODY, 0); }
		public TerminalNode IN() { return getToken(CuneiformParser.IN, 0); }
		public ForeignBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_foreignBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).enterForeignBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof CuneiformListener ) ((CuneiformListener)listener).exitForeignBody(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof CuneiformVisitor ) return ((CuneiformVisitor<? extends T>)visitor).visitForeignBody(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ForeignBodyContext foreignBody() throws RecognitionException {
		ForeignBodyContext _localctx = new ForeignBodyContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_foreignBody);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(402); match(IN);
			setState(403); match(ID);
			setState(404); match(BODY);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3&\u0199\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\3\2\7\2.\n\2\f\2\16\2\61\13\2"+
		"\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\5\3;\n\3\3\4\3\4\3\4\5\4@\n\4\3\5\3\5"+
		"\3\5\3\5\3\6\6\6G\n\6\r\6\16\6H\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\6"+
		"\7T\n\7\r\7\16\7U\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7"+
		"\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\6"+
		"\7v\n\7\r\7\16\7w\3\7\3\7\7\7|\n\7\f\7\16\7\177\13\7\3\7\3\7\3\7\3\7\3"+
		"\7\3\7\6\7\u0087\n\7\r\7\16\7\u0088\3\7\3\7\7\7\u008d\n\7\f\7\16\7\u0090"+
		"\13\7\3\7\3\7\6\7\u0094\n\7\r\7\16\7\u0095\3\7\3\7\5\7\u009a\n\7\3\b\3"+
		"\b\6\b\u009e\n\b\r\b\16\b\u009f\3\b\3\b\7\b\u00a4\n\b\f\b\16\b\u00a7\13"+
		"\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\5\t"+
		"\u00b9\n\t\3\t\3\t\3\t\3\t\3\t\3\t\6\t\u00c1\n\t\r\t\16\t\u00c2\3\t\5"+
		"\t\u00c6\n\t\3\n\3\n\3\n\3\n\5\n\u00cc\n\n\3\13\3\13\3\13\3\13\3\13\3"+
		"\13\5\13\u00d4\n\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\5\13\u00de"+
		"\n\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\5\13\u00e8\n\13\3\13\3\13"+
		"\3\13\3\13\3\13\3\13\3\13\3\13\5\13\u00f2\n\13\3\13\3\13\5\13\u00f6\n"+
		"\13\3\f\3\f\5\f\u00fa\n\f\3\r\3\r\3\r\3\r\3\16\3\16\3\16\6\16\u0103\n"+
		"\16\r\16\16\16\u0104\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16"+
		"\5\16\u0111\n\16\3\17\3\17\6\17\u0115\n\17\r\17\16\17\u0116\5\17\u0119"+
		"\n\17\3\20\3\20\3\20\3\21\3\21\3\21\3\21\3\21\5\21\u0123\n\21\3\21\3\21"+
		"\3\21\3\21\3\21\3\21\3\21\3\21\3\21\5\21\u012e\n\21\3\21\3\21\3\21\6\21"+
		"\u0133\n\21\r\21\16\21\u0134\3\21\5\21\u0138\n\21\3\21\3\21\3\21\5\21"+
		"\u013d\n\21\3\21\3\21\3\21\7\21\u0142\n\21\f\21\16\21\u0145\13\21\3\21"+
		"\5\21\u0148\n\21\3\21\3\21\3\21\3\21\6\21\u014e\n\21\r\21\16\21\u014f"+
		"\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21"+
		"\3\21\3\21\3\21\3\21\6\21\u0164\n\21\r\21\16\21\u0165\3\21\3\21\3\21\3"+
		"\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3"+
		"\21\3\21\5\21\u017b\n\21\3\22\3\22\3\22\3\22\3\23\3\23\6\23\u0183\n\23"+
		"\r\23\16\23\u0184\3\23\3\23\3\23\3\23\3\23\5\23\u018c\n\23\3\24\3\24\3"+
		"\24\3\24\3\25\3\25\3\25\3\26\3\26\3\26\3\26\3\26\2\2\27\2\4\6\b\n\f\16"+
		"\20\22\24\26\30\32\34\36 \"$&(*\2\3\3\2\16\17\u01ca\2/\3\2\2\2\4:\3\2"+
		"\2\2\6?\3\2\2\2\bA\3\2\2\2\nF\3\2\2\2\f\u0099\3\2\2\2\16\u009b\3\2\2\2"+
		"\20\u00c5\3\2\2\2\22\u00cb\3\2\2\2\24\u00f5\3\2\2\2\26\u00f9\3\2\2\2\30"+
		"\u00fb\3\2\2\2\32\u0110\3\2\2\2\34\u0118\3\2\2\2\36\u011a\3\2\2\2 \u017a"+
		"\3\2\2\2\"\u017c\3\2\2\2$\u018b\3\2\2\2&\u018d\3\2\2\2(\u0191\3\2\2\2"+
		"*\u0194\3\2\2\2,.\5\4\3\2-,\3\2\2\2.\61\3\2\2\2/-\3\2\2\2/\60\3\2\2\2"+
		"\60\62\3\2\2\2\61/\3\2\2\2\62\63\7\2\2\3\63\3\3\2\2\2\64;\5(\25\2\65;"+
		"\5\b\5\2\66;\5\6\4\2\678\5\34\17\289\b\3\1\29;\3\2\2\2:\64\3\2\2\2:\65"+
		"\3\2\2\2:\66\3\2\2\2:\67\3\2\2\2;\5\3\2\2\2<@\5\n\6\2=@\5\f\7\2>@\5\36"+
		"\20\2?<\3\2\2\2?=\3\2\2\2?>\3\2\2\2@\7\3\2\2\2AB\t\2\2\2BC\7#\2\2CD\7"+
		"\33\2\2D\t\3\2\2\2EG\5\20\t\2FE\3\2\2\2GH\3\2\2\2HF\3\2\2\2HI\3\2\2\2"+
		"IJ\3\2\2\2JK\7\t\2\2KL\5\34\17\2LM\7\33\2\2M\13\3\2\2\2NO\7\b\2\2OP\7"+
		"%\2\2PQ\5\16\b\2QS\7\21\2\2RT\5\6\4\2SR\3\2\2\2TU\3\2\2\2US\3\2\2\2UV"+
		"\3\2\2\2VW\3\2\2\2WX\7\27\2\2X\u009a\3\2\2\2YZ\7\b\2\2Z[\7%\2\2[\\\5\16"+
		"\b\2\\]\5*\26\2]\u009a\3\2\2\2^_\7\b\2\2_\u009a\b\7\1\2`a\7\b\2\2ab\7"+
		"%\2\2bc\5\16\b\2cd\7\21\2\2de\b\7\1\2e\u009a\3\2\2\2fg\7\b\2\2gh\7%\2"+
		"\2hi\5\16\b\2ij\7\21\2\2jk\7\27\2\2kl\b\7\1\2l\u009a\3\2\2\2mn\7\b\2\2"+
		"no\7%\2\2op\7\22\2\2p\u009a\b\7\1\2qr\7\b\2\2rs\7%\2\2su\7\22\2\2tv\5"+
		"\26\f\2ut\3\2\2\2vw\3\2\2\2wu\3\2\2\2wx\3\2\2\2xy\3\2\2\2y}\7\4\2\2z|"+
		"\5\22\n\2{z\3\2\2\2|\177\3\2\2\2}{\3\2\2\2}~\3\2\2\2~\u0080\3\2\2\2\177"+
		"}\3\2\2\2\u0080\u0081\b\7\1\2\u0081\u009a\3\2\2\2\u0082\u0083\7\b\2\2"+
		"\u0083\u0084\7%\2\2\u0084\u0086\7\22\2\2\u0085\u0087\5\26\f\2\u0086\u0085"+
		"\3\2\2\2\u0087\u0088\3\2\2\2\u0088\u0086\3\2\2\2\u0088\u0089\3\2\2\2\u0089"+
		"\u008a\3\2\2\2\u008a\u008e\7\4\2\2\u008b\u008d\5\22\n\2\u008c\u008b\3"+
		"\2\2\2\u008d\u0090\3\2\2\2\u008e\u008c\3\2\2\2\u008e\u008f\3\2\2\2\u008f"+
		"\u0091\3\2\2\2\u0090\u008e\3\2\2\2\u0091\u0093\7\30\2\2\u0092\u0094\7"+
		"\30\2\2\u0093\u0092\3\2\2\2\u0094\u0095\3\2\2\2\u0095\u0093\3\2\2\2\u0095"+
		"\u0096\3\2\2\2\u0096\u0097\3\2\2\2\u0097\u0098\b\7\1\2\u0098\u009a\3\2"+
		"\2\2\u0099N\3\2\2\2\u0099Y\3\2\2\2\u0099^\3\2\2\2\u0099`\3\2\2\2\u0099"+
		"f\3\2\2\2\u0099m\3\2\2\2\u0099q\3\2\2\2\u0099\u0082\3\2\2\2\u009a\r\3"+
		"\2\2\2\u009b\u009d\7\22\2\2\u009c\u009e\5\26\f\2\u009d\u009c\3\2\2\2\u009e"+
		"\u009f\3\2\2\2\u009f\u009d\3\2\2\2\u009f\u00a0\3\2\2\2\u00a0\u00a1\3\2"+
		"\2\2\u00a1\u00a5\7\4\2\2\u00a2\u00a4\5\22\n\2\u00a3\u00a2\3\2\2\2\u00a4"+
		"\u00a7\3\2\2\2\u00a5\u00a3\3\2\2\2\u00a5\u00a6\3\2\2\2\u00a6\u00a8\3\2"+
		"\2\2\u00a7\u00a5\3\2\2\2\u00a8\u00a9\7\30\2\2\u00a9\17\3\2\2\2\u00aa\u00c6"+
		"\7%\2\2\u00ab\u00ac\7%\2\2\u00ac\u00ad\7\22\2\2\u00ad\u00ae\7%\2\2\u00ae"+
		"\u00c6\7\30\2\2\u00af\u00b0\7%\2\2\u00b0\u00b1\7\22\2\2\u00b1\u00b2\7"+
		"\4\2\2\u00b2\u00c6\7\30\2\2\u00b3\u00b4\7%\2\2\u00b4\u00c6\5\16\b\2\u00b5"+
		"\u00b6\7%\2\2\u00b6\u00b8\7\22\2\2\u00b7\u00b9\7\4\2\2\u00b8\u00b7\3\2"+
		"\2\2\u00b8\u00b9\3\2\2\2\u00b9\u00ba\3\2\2\2\u00ba\u00c6\b\t\1\2\u00bb"+
		"\u00bc\7%\2\2\u00bc\u00bd\7\22\2\2\u00bd\u00be\7%\2\2\u00be\u00c0\7\30"+
		"\2\2\u00bf\u00c1\7\30\2\2\u00c0\u00bf\3\2\2\2\u00c1\u00c2\3\2\2\2\u00c2"+
		"\u00c0\3\2\2\2\u00c2\u00c3\3\2\2\2\u00c3\u00c4\3\2\2\2\u00c4\u00c6\b\t"+
		"\1\2\u00c5\u00aa\3\2\2\2\u00c5\u00ab\3\2\2\2\u00c5\u00af\3\2\2\2\u00c5"+
		"\u00b3\3\2\2\2\u00c5\u00b5\3\2\2\2\u00c5\u00bb\3\2\2\2\u00c6\21\3\2\2"+
		"\2\u00c7\u00cc\5\20\t\2\u00c8\u00cc\5\30\r\2\u00c9\u00cc\5\32\16\2\u00ca"+
		"\u00cc\5\24\13\2\u00cb\u00c7\3\2\2\2\u00cb\u00c8\3\2\2\2\u00cb\u00c9\3"+
		"\2\2\2\u00cb\u00ca\3\2\2\2\u00cc\23\3\2\2\2\u00cd\u00ce\7\21\2\2\u00ce"+
		"\u00cf\7\5\2\2\u00cf\u00d0\7 \2\2\u00d0\u00d3\7\4\2\2\u00d1\u00d4\5\20"+
		"\t\2\u00d2\u00d4\5\32\16\2\u00d3\u00d1\3\2\2\2\u00d3\u00d2\3\2\2\2\u00d4"+
		"\u00d5\3\2\2\2\u00d5\u00d6\7\27\2\2\u00d6\u00f6\3\2\2\2\u00d7\u00d8\7"+
		"\21\2\2\u00d8\u00d9\7\6\2\2\u00d9\u00da\7 \2\2\u00da\u00dd\7\4\2\2\u00db"+
		"\u00de\5\20\t\2\u00dc\u00de\5\32\16\2\u00dd\u00db\3\2\2\2\u00dd\u00dc"+
		"\3\2\2\2\u00de\u00df\3\2\2\2\u00df\u00e0\7\27\2\2\u00e0\u00f6\3\2\2\2"+
		"\u00e1\u00e2\7\21\2\2\u00e2\u00e3\7\37\2\2\u00e3\u00e4\7 \2\2\u00e4\u00e7"+
		"\7\4\2\2\u00e5\u00e8\5\20\t\2\u00e6\u00e8\5\32\16\2\u00e7\u00e5\3\2\2"+
		"\2\u00e7\u00e6\3\2\2\2\u00e8\u00e9\3\2\2\2\u00e9\u00ea\7\27\2\2\u00ea"+
		"\u00f6\3\2\2\2\u00eb\u00ec\7\21\2\2\u00ec\u00ed\7\26\2\2\u00ed\u00ee\7"+
		" \2\2\u00ee\u00f1\7\4\2\2\u00ef\u00f2\5\20\t\2\u00f0\u00f2\5\32\16\2\u00f1"+
		"\u00ef\3\2\2\2\u00f1\u00f0\3\2\2\2\u00f2\u00f3\3\2\2\2\u00f3\u00f4\7\27"+
		"\2\2\u00f4\u00f6\3\2\2\2\u00f5\u00cd\3\2\2\2\u00f5\u00d7\3\2\2\2\u00f5"+
		"\u00e1\3\2\2\2\u00f5\u00eb\3\2\2\2\u00f6\25\3\2\2\2\u00f7\u00fa\5\20\t"+
		"\2\u00f8\u00fa\5\30\r\2\u00f9\u00f7\3\2\2\2\u00f9\u00f8\3\2\2\2\u00fa"+
		"\27\3\2\2\2\u00fb\u00fc\7\24\2\2\u00fc\u00fd\5\20\t\2\u00fd\u00fe\7\32"+
		"\2\2\u00fe\31\3\2\2\2\u00ff\u0100\7\23\2\2\u0100\u0102\5\20\t\2\u0101"+
		"\u0103\5\20\t\2\u0102\u0101\3\2\2\2\u0103\u0104\3\2\2\2\u0104\u0102\3"+
		"\2\2\2\u0104\u0105\3\2\2\2\u0105\u0106\3\2\2\2\u0106\u0107\7\31\2\2\u0107"+
		"\u0111\3\2\2\2\u0108\u0109\7\23\2\2\u0109\u010a\5\20\t\2\u010a\u010b\7"+
		"\31\2\2\u010b\u010c\b\16\1\2\u010c\u0111\3\2\2\2\u010d\u010e\7\23\2\2"+
		"\u010e\u010f\7\31\2\2\u010f\u0111\b\16\1\2\u0110\u00ff\3\2\2\2\u0110\u0108"+
		"\3\2\2\2\u0110\u010d\3\2\2\2\u0111\33\3\2\2\2\u0112\u0119\7\25\2\2\u0113"+
		"\u0115\5 \21\2\u0114\u0113\3\2\2\2\u0115\u0116\3\2\2\2\u0116\u0114\3\2"+
		"\2\2\u0116\u0117\3\2\2\2\u0117\u0119\3\2\2\2\u0118\u0112\3\2\2\2\u0118"+
		"\u0114\3\2\2\2\u0119\35\3\2\2\2\u011a\u011b\5\34\17\2\u011b\u011c\7\36"+
		"\2\2\u011c\37\3\2\2\2\u011d\u017b\7%\2\2\u011e\u017b\7 \2\2\u011f\u017b"+
		"\7#\2\2\u0120\u017b\7\13\2\2\u0121\u0123\5\"\22\2\u0122\u0121\3\2\2\2"+
		"\u0122\u0123\3\2\2\2\u0123\u0124\3\2\2\2\u0124\u0125\7\f\2\2\u0125\u0126"+
		"\5\16\b\2\u0126\u0127\5\34\17\2\u0127\u0128\7\35\2\2\u0128\u0129\5$\23"+
		"\2\u0129\u012a\7\n\2\2\u012a\u012b\5$\23\2\u012b\u017b\3\2\2\2\u012c\u012e"+
		"\5\"\22\2\u012d\u012c\3\2\2\2\u012d\u012e\3\2\2\2\u012e\u012f\3\2\2\2"+
		"\u012f\u0130\7\3\2\2\u0130\u0132\7\22\2\2\u0131\u0133\5&\24\2\u0132\u0131"+
		"\3\2\2\2\u0133\u0134\3\2\2\2\u0134\u0132\3\2\2\2\u0134\u0135\3\2\2\2\u0135"+
		"\u0137\3\2\2\2\u0136\u0138\7\34\2\2\u0137\u0136\3\2\2\2\u0137\u0138\3"+
		"\2\2\2\u0138\u0139\3\2\2\2\u0139\u013a\7\30\2\2\u013a\u017b\3\2\2\2\u013b"+
		"\u013d\5\"\22\2\u013c\u013b\3\2\2\2\u013c\u013d\3\2\2\2\u013d\u013e\3"+
		"\2\2\2\u013e\u013f\7%\2\2\u013f\u0143\7\22\2\2\u0140\u0142\5&\24\2\u0141"+
		"\u0140\3\2\2\2\u0142\u0145\3\2\2\2\u0143\u0141\3\2\2\2\u0143\u0144\3\2"+
		"\2\2\u0144\u0147\3\2\2\2\u0145\u0143\3\2\2\2\u0146\u0148\7\34\2\2\u0147"+
		"\u0146\3\2\2\2\u0147\u0148\3\2\2\2\u0148\u0149\3\2\2\2\u0149\u017b\7\30"+
		"\2\2\u014a\u014b\7\7\2\2\u014b\u014d\7\22\2\2\u014c\u014e\5&\24\2\u014d"+
		"\u014c\3\2\2\2\u014e\u014f\3\2\2\2\u014f\u014d\3\2\2\2\u014f\u0150\3\2"+
		"\2\2\u0150\u0151\3\2\2\2\u0151\u0152\7\30\2\2\u0152\u017b\3\2\2\2\u0153"+
		"\u0154\7\20\2\2\u0154\u0155\5\16\b\2\u0155\u0156\5$\23\2\u0156\u017b\3"+
		"\2\2\2\u0157\u0158\7\20\2\2\u0158\u0159\5\16\b\2\u0159\u015a\5*\26\2\u015a"+
		"\u017b\3\2\2\2\u015b\u015c\7\3\2\2\u015c\u017b\b\21\1\2\u015d\u015e\7"+
		"\3\2\2\u015e\u015f\7\22\2\2\u015f\u017b\b\21\1\2\u0160\u0161\7\3\2\2\u0161"+
		"\u0163\7\22\2\2\u0162\u0164\5&\24\2\u0163\u0162\3\2\2\2\u0164\u0165\3"+
		"\2\2\2\u0165\u0163\3\2\2\2\u0165\u0166\3\2\2\2\u0166\u0167\3\2\2\2\u0167"+
		"\u0168\b\21\1\2\u0168\u017b\3\2\2\2\u0169\u016a\7\3\2\2\u016a\u016b\7"+
		"\22\2\2\u016b\u016c\7%\2\2\u016c\u016d\7\4\2\2\u016d\u017b\b\21\1\2\u016e"+
		"\u016f\7\20\2\2\u016f\u0170\5\16\b\2\u0170\u0171\7\r\2\2\u0171\u0172\b"+
		"\21\1\2\u0172\u017b\3\2\2\2\u0173\u0174\7\20\2\2\u0174\u0175\5\16\b\2"+
		"\u0175\u0176\7\r\2\2\u0176\u0177\7%\2\2\u0177\u0178\7\"\2\2\u0178\u0179"+
		"\b\21\1\2\u0179\u017b\3\2\2\2\u017a\u011d\3\2\2\2\u017a\u011e\3\2\2\2"+
		"\u017a\u011f\3\2\2\2\u017a\u0120\3\2\2\2\u017a\u0122\3\2\2\2\u017a\u012d"+
		"\3\2\2\2\u017a\u013c\3\2\2\2\u017a\u014a\3\2\2\2\u017a\u0153\3\2\2\2\u017a"+
		"\u0157\3\2\2\2\u017a\u015b\3\2\2\2\u017a\u015d\3\2\2\2\u017a\u0160\3\2"+
		"\2\2\u017a\u0169\3\2\2\2\u017a\u016e\3\2\2\2\u017a\u0173\3\2\2\2\u017b"+
		"!\3\2\2\2\u017c\u017d\7\23\2\2\u017d\u017e\7 \2\2\u017e\u017f\7\31\2\2"+
		"\u017f#\3\2\2\2\u0180\u0182\7\21\2\2\u0181\u0183\5\6\4\2\u0182\u0181\3"+
		"\2\2\2\u0183\u0184\3\2\2\2\u0184\u0182\3\2\2\2\u0184\u0185\3\2\2\2\u0185"+
		"\u0186\3\2\2\2\u0186\u0187\7\27\2\2\u0187\u018c\3\2\2\2\u0188\u0189\7"+
		"\21\2\2\u0189\u018a\7\27\2\2\u018a\u018c\b\23\1\2\u018b\u0180\3\2\2\2"+
		"\u018b\u0188\3\2\2\2\u018c%\3\2\2\2\u018d\u018e\7%\2\2\u018e\u018f\7\4"+
		"\2\2\u018f\u0190\5\34\17\2\u0190\'\3\2\2\2\u0191\u0192\5\34\17\2\u0192"+
		"\u0193\7\33\2\2\u0193)\3\2\2\2\u0194\u0195\7\r\2\2\u0195\u0196\7%\2\2"+
		"\u0196\u0197\7!\2\2\u0197+\3\2\2\2)/:?HUw}\u0088\u008e\u0095\u0099\u009f"+
		"\u00a5\u00b8\u00c2\u00c5\u00cb\u00d3\u00dd\u00e7\u00f1\u00f5\u00f9\u0104"+
		"\u0110\u0116\u0118\u0122\u012d\u0134\u0137\u013c\u0143\u0147\u014f\u0165"+
		"\u017a\u0184\u018b";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}