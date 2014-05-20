// Generated from de/huberlin/cuneiform/libdax/parser/Dax.g4 by ANTLR 4.2
package de.huberlin.cuneiform.libdax.parser;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class DaxParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ADAG=1, ARGUMENT=2, CHILD=3, COUNT=4, DVNAME=5, DVVERSION=6, EQ=7, EXECUTABLE=8, 
		FALSE=9, FILE=10, FILENAME=11, ID=12, INDEX=13, INOUT=14, INPUT=15, JOB=16, 
		LARGUMENT=17, LEVEL=18, LINK=19, LTAG=20, NAME=21, OPTIONAL=22, OUTPUT=23, 
		PARENT=24, RARGUMENT=25, REF=26, REGISTER=27, RTAG=28, SCHEMALOCATION=29, 
		SLASH=30, TRANSFER=31, TRUE=32, TYPE=33, USES=34, VERSION=35, XMLNS=36, 
		XSI=37, STRING=38, ARG=39, METAINFO=40, COMMENT=41, WS=42;
	public static final String[] tokenNames = {
		"<INVALID>", "'adag'", "'argument'", "'child'", "'count'", "'dv-name'", 
		"'dv-version'", "'='", "'\"executable\"'", "'\"false\"'", "'file'", "'filename'", 
		"'id'", "'index'", "'\"inout\"'", "'\"input\"'", "'job'", "'<argument>'", 
		"'level'", "'link'", "'<'", "'name'", "'optional'", "'\"output\"'", "'parent'", 
		"'</argument>'", "'ref'", "'register'", "'>'", "'xsi:schemaLocation'", 
		"'/'", "'transfer'", "'\"true\"'", "'type'", "'uses'", "'version'", "'xmlns'", 
		"'xmlns:xsi'", "STRING", "ARG", "METAINFO", "COMMENT", "WS"
	};
	public static final int
		RULE_adag = 0, RULE_adagProp = 1, RULE_adagEl = 2, RULE_filename = 3, 
		RULE_filenameProp = 4, RULE_job = 5, RULE_jobProp = 6, RULE_jobEl = 7, 
		RULE_argument = 8, RULE_argumentEl = 9, RULE_jobUsesProp = 10, RULE_child = 11, 
		RULE_parent = 12;
	public static final String[] ruleNames = {
		"adag", "adagProp", "adagEl", "filename", "filenameProp", "job", "jobProp", 
		"jobEl", "argument", "argumentEl", "jobUsesProp", "child", "parent"
	};

	@Override
	public String getGrammarFileName() { return "Dax.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public DaxParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class AdagContext extends ParserRuleContext {
		public List<TerminalNode> LTAG() { return getTokens(DaxParser.LTAG); }
		public AdagElContext adagEl(int i) {
			return getRuleContext(AdagElContext.class,i);
		}
		public List<TerminalNode> ADAG() { return getTokens(DaxParser.ADAG); }
		public TerminalNode LTAG(int i) {
			return getToken(DaxParser.LTAG, i);
		}
		public TerminalNode ADAG(int i) {
			return getToken(DaxParser.ADAG, i);
		}
		public List<TerminalNode> RTAG() { return getTokens(DaxParser.RTAG); }
		public TerminalNode SLASH() { return getToken(DaxParser.SLASH, 0); }
		public List<AdagPropContext> adagProp() {
			return getRuleContexts(AdagPropContext.class);
		}
		public AdagPropContext adagProp(int i) {
			return getRuleContext(AdagPropContext.class,i);
		}
		public List<AdagElContext> adagEl() {
			return getRuleContexts(AdagElContext.class);
		}
		public TerminalNode RTAG(int i) {
			return getToken(DaxParser.RTAG, i);
		}
		public AdagContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_adag; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterAdag(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitAdag(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitAdag(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AdagContext adag() throws RecognitionException {
		AdagContext _localctx = new AdagContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_adag);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(26); match(LTAG);
			setState(27); match(ADAG);
			setState(31);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << COUNT) | (1L << INDEX) | (1L << NAME) | (1L << SCHEMALOCATION) | (1L << VERSION) | (1L << XMLNS) | (1L << XSI))) != 0)) {
				{
				{
				setState(28); adagProp();
				}
				}
				setState(33);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(34); match(RTAG);
			setState(38);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					{
					{
					setState(35); adagEl();
					}
					} 
				}
				setState(40);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(41); match(LTAG);
			setState(42); match(SLASH);
			setState(43); match(ADAG);
			setState(44); match(RTAG);
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

	public static class AdagPropContext extends ParserRuleContext {
		public TerminalNode COUNT() { return getToken(DaxParser.COUNT, 0); }
		public TerminalNode INDEX() { return getToken(DaxParser.INDEX, 0); }
		public TerminalNode VERSION() { return getToken(DaxParser.VERSION, 0); }
		public TerminalNode SCHEMALOCATION() { return getToken(DaxParser.SCHEMALOCATION, 0); }
		public TerminalNode NAME() { return getToken(DaxParser.NAME, 0); }
		public TerminalNode XMLNS() { return getToken(DaxParser.XMLNS, 0); }
		public TerminalNode XSI() { return getToken(DaxParser.XSI, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public AdagPropContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_adagProp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterAdagProp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitAdagProp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitAdagProp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AdagPropContext adagProp() throws RecognitionException {
		AdagPropContext _localctx = new AdagPropContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_adagProp);
		try {
			setState(67);
			switch (_input.LA(1)) {
			case XMLNS:
				enterOuterAlt(_localctx, 1);
				{
				setState(46); match(XMLNS);
				setState(47); match(EQ);
				setState(48); match(STRING);
				}
				break;
			case XSI:
				enterOuterAlt(_localctx, 2);
				{
				setState(49); match(XSI);
				setState(50); match(EQ);
				setState(51); match(STRING);
				}
				break;
			case SCHEMALOCATION:
				enterOuterAlt(_localctx, 3);
				{
				setState(52); match(SCHEMALOCATION);
				setState(53); match(EQ);
				setState(54); match(STRING);
				}
				break;
			case VERSION:
				enterOuterAlt(_localctx, 4);
				{
				setState(55); match(VERSION);
				setState(56); match(EQ);
				setState(57); match(STRING);
				}
				break;
			case COUNT:
				enterOuterAlt(_localctx, 5);
				{
				setState(58); match(COUNT);
				setState(59); match(EQ);
				setState(60); match(STRING);
				}
				break;
			case INDEX:
				enterOuterAlt(_localctx, 6);
				{
				setState(61); match(INDEX);
				setState(62); match(EQ);
				setState(63); match(STRING);
				}
				break;
			case NAME:
				enterOuterAlt(_localctx, 7);
				{
				setState(64); match(NAME);
				setState(65); match(EQ);
				setState(66); match(STRING);
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

	public static class AdagElContext extends ParserRuleContext {
		public ChildContext child() {
			return getRuleContext(ChildContext.class,0);
		}
		public FilenameContext filename() {
			return getRuleContext(FilenameContext.class,0);
		}
		public JobContext job() {
			return getRuleContext(JobContext.class,0);
		}
		public AdagElContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_adagEl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterAdagEl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitAdagEl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitAdagEl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AdagElContext adagEl() throws RecognitionException {
		AdagElContext _localctx = new AdagElContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_adagEl);
		try {
			setState(72);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(69); filename();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(70); job();
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(71); child();
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

	public static class FilenameContext extends ParserRuleContext {
		public TerminalNode LTAG() { return getToken(DaxParser.LTAG, 0); }
		public TerminalNode FILENAME() { return getToken(DaxParser.FILENAME, 0); }
		public TerminalNode SLASH() { return getToken(DaxParser.SLASH, 0); }
		public TerminalNode RTAG() { return getToken(DaxParser.RTAG, 0); }
		public List<FilenamePropContext> filenameProp() {
			return getRuleContexts(FilenamePropContext.class);
		}
		public FilenamePropContext filenameProp(int i) {
			return getRuleContext(FilenamePropContext.class,i);
		}
		public FilenameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_filename; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterFilename(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitFilename(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitFilename(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FilenameContext filename() throws RecognitionException {
		FilenameContext _localctx = new FilenameContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_filename);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(74); match(LTAG);
			setState(75); match(FILENAME);
			setState(79);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==FILE || _la==LINK) {
				{
				{
				setState(76); filenameProp();
				}
				}
				setState(81);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(82); match(SLASH);
			setState(83); match(RTAG);
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

	public static class FilenamePropContext extends ParserRuleContext {
		public FilenamePropContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_filenameProp; }
	 
		public FilenamePropContext() { }
		public void copyFrom(FilenamePropContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class FilenamePropLinkContext extends FilenamePropContext {
		public TerminalNode OUTPUT() { return getToken(DaxParser.OUTPUT, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode INPUT() { return getToken(DaxParser.INPUT, 0); }
		public TerminalNode INOUT() { return getToken(DaxParser.INOUT, 0); }
		public TerminalNode LINK() { return getToken(DaxParser.LINK, 0); }
		public FilenamePropLinkContext(FilenamePropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterFilenamePropLink(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitFilenamePropLink(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitFilenamePropLink(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class FilenamePropFileContext extends FilenamePropContext {
		public TerminalNode FILE() { return getToken(DaxParser.FILE, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public FilenamePropFileContext(FilenamePropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterFilenamePropFile(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitFilenamePropFile(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitFilenamePropFile(this);
			else return visitor.visitChildren(this);
		}
	}

	public final FilenamePropContext filenameProp() throws RecognitionException {
		FilenamePropContext _localctx = new FilenamePropContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_filenameProp);
		int _la;
		try {
			setState(91);
			switch (_input.LA(1)) {
			case FILE:
				_localctx = new FilenamePropFileContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(85); match(FILE);
				setState(86); match(EQ);
				setState(87); match(STRING);
				}
				break;
			case LINK:
				_localctx = new FilenamePropLinkContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(88); match(LINK);
				setState(89); match(EQ);
				setState(90);
				_la = _input.LA(1);
				if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << INOUT) | (1L << INPUT) | (1L << OUTPUT))) != 0)) ) {
				_errHandler.recoverInline(this);
				}
				consume();
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

	public static class JobContext extends ParserRuleContext {
		public List<JobPropContext> jobProp() {
			return getRuleContexts(JobPropContext.class);
		}
		public List<TerminalNode> LTAG() { return getTokens(DaxParser.LTAG); }
		public JobPropContext jobProp(int i) {
			return getRuleContext(JobPropContext.class,i);
		}
		public TerminalNode LTAG(int i) {
			return getToken(DaxParser.LTAG, i);
		}
		public TerminalNode JOB(int i) {
			return getToken(DaxParser.JOB, i);
		}
		public List<TerminalNode> RTAG() { return getTokens(DaxParser.RTAG); }
		public TerminalNode SLASH() { return getToken(DaxParser.SLASH, 0); }
		public List<JobElContext> jobEl() {
			return getRuleContexts(JobElContext.class);
		}
		public List<TerminalNode> JOB() { return getTokens(DaxParser.JOB); }
		public TerminalNode RTAG(int i) {
			return getToken(DaxParser.RTAG, i);
		}
		public JobElContext jobEl(int i) {
			return getRuleContext(JobElContext.class,i);
		}
		public JobContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_job; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJob(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJob(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJob(this);
			else return visitor.visitChildren(this);
		}
	}

	public final JobContext job() throws RecognitionException {
		JobContext _localctx = new JobContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_job);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(93); match(LTAG);
			setState(94); match(JOB);
			setState(98);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << DVNAME) | (1L << DVVERSION) | (1L << ID) | (1L << LEVEL) | (1L << NAME) | (1L << VERSION))) != 0)) {
				{
				{
				setState(95); jobProp();
				}
				}
				setState(100);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(101); match(RTAG);
			setState(105);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,7,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					{
					{
					setState(102); jobEl();
					}
					} 
				}
				setState(107);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,7,_ctx);
			}
			setState(108); match(LTAG);
			setState(109); match(SLASH);
			setState(110); match(JOB);
			setState(111); match(RTAG);
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

	public static class JobPropContext extends ParserRuleContext {
		public JobPropContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_jobProp; }
	 
		public JobPropContext() { }
		public void copyFrom(JobPropContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class JobPropIdContext extends JobPropContext {
		public TerminalNode ID() { return getToken(DaxParser.ID, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public JobPropIdContext(JobPropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobPropId(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobPropId(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobPropId(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class JobPropDvnameContext extends JobPropContext {
		public TerminalNode DVNAME() { return getToken(DaxParser.DVNAME, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public JobPropDvnameContext(JobPropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobPropDvname(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobPropDvname(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobPropDvname(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class JobPropNameContext extends JobPropContext {
		public TerminalNode NAME() { return getToken(DaxParser.NAME, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public JobPropNameContext(JobPropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobPropName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobPropName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobPropName(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class JobPropDvversionContext extends JobPropContext {
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public TerminalNode DVVERSION() { return getToken(DaxParser.DVVERSION, 0); }
		public JobPropDvversionContext(JobPropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobPropDvversion(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobPropDvversion(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobPropDvversion(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class JobPropLevelContext extends JobPropContext {
		public TerminalNode LEVEL() { return getToken(DaxParser.LEVEL, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public JobPropLevelContext(JobPropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobPropLevel(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobPropLevel(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobPropLevel(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class JobPropVersionContext extends JobPropContext {
		public TerminalNode VERSION() { return getToken(DaxParser.VERSION, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public JobPropVersionContext(JobPropContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobPropVersion(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobPropVersion(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobPropVersion(this);
			else return visitor.visitChildren(this);
		}
	}

	public final JobPropContext jobProp() throws RecognitionException {
		JobPropContext _localctx = new JobPropContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_jobProp);
		try {
			setState(131);
			switch (_input.LA(1)) {
			case ID:
				_localctx = new JobPropIdContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(113); match(ID);
				setState(114); match(EQ);
				setState(115); match(STRING);
				}
				break;
			case NAME:
				_localctx = new JobPropNameContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(116); match(NAME);
				setState(117); match(EQ);
				setState(118); match(STRING);
				}
				break;
			case VERSION:
				_localctx = new JobPropVersionContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(119); match(VERSION);
				setState(120); match(EQ);
				setState(121); match(STRING);
				}
				break;
			case LEVEL:
				_localctx = new JobPropLevelContext(_localctx);
				enterOuterAlt(_localctx, 4);
				{
				setState(122); match(LEVEL);
				setState(123); match(EQ);
				setState(124); match(STRING);
				}
				break;
			case DVNAME:
				_localctx = new JobPropDvnameContext(_localctx);
				enterOuterAlt(_localctx, 5);
				{
				setState(125); match(DVNAME);
				setState(126); match(EQ);
				setState(127); match(STRING);
				}
				break;
			case DVVERSION:
				_localctx = new JobPropDvversionContext(_localctx);
				enterOuterAlt(_localctx, 6);
				{
				setState(128); match(DVVERSION);
				setState(129); match(EQ);
				setState(130); match(STRING);
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

	public static class JobElContext extends ParserRuleContext {
		public TerminalNode LTAG() { return getToken(DaxParser.LTAG, 0); }
		public TerminalNode SLASH() { return getToken(DaxParser.SLASH, 0); }
		public TerminalNode RTAG() { return getToken(DaxParser.RTAG, 0); }
		public List<JobUsesPropContext> jobUsesProp() {
			return getRuleContexts(JobUsesPropContext.class);
		}
		public TerminalNode USES() { return getToken(DaxParser.USES, 0); }
		public JobUsesPropContext jobUsesProp(int i) {
			return getRuleContext(JobUsesPropContext.class,i);
		}
		public ArgumentContext argument() {
			return getRuleContext(ArgumentContext.class,0);
		}
		public JobElContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_jobEl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobEl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobEl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobEl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final JobElContext jobEl() throws RecognitionException {
		JobElContext _localctx = new JobElContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_jobEl);
		int _la;
		try {
			setState(144);
			switch (_input.LA(1)) {
			case LARGUMENT:
				enterOuterAlt(_localctx, 1);
				{
				setState(133); argument();
				}
				break;
			case LTAG:
				enterOuterAlt(_localctx, 2);
				{
				setState(134); match(LTAG);
				setState(135); match(USES);
				setState(139);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << FILE) | (1L << LINK) | (1L << OPTIONAL) | (1L << REGISTER) | (1L << TRANSFER) | (1L << TYPE))) != 0)) {
					{
					{
					setState(136); jobUsesProp();
					}
					}
					setState(141);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(142); match(SLASH);
				setState(143); match(RTAG);
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

	public static class ArgumentContext extends ParserRuleContext {
		public ArgumentElContext argumentEl(int i) {
			return getRuleContext(ArgumentElContext.class,i);
		}
		public TerminalNode RARGUMENT() { return getToken(DaxParser.RARGUMENT, 0); }
		public List<ArgumentElContext> argumentEl() {
			return getRuleContexts(ArgumentElContext.class);
		}
		public TerminalNode LARGUMENT() { return getToken(DaxParser.LARGUMENT, 0); }
		public ArgumentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argument; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterArgument(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitArgument(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitArgument(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgumentContext argument() throws RecognitionException {
		ArgumentContext _localctx = new ArgumentContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_argument);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(146); match(LARGUMENT);
			setState(150);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==LTAG || _la==ARG) {
				{
				{
				setState(147); argumentEl();
				}
				}
				setState(152);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(153); match(RARGUMENT);
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

	public static class ArgumentElContext extends ParserRuleContext {
		public ArgumentElContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argumentEl; }
	 
		public ArgumentElContext() { }
		public void copyFrom(ArgumentElContext ctx) {
			super.copyFrom(ctx);
		}
	}
	public static class ArgumentElPlainContext extends ArgumentElContext {
		public TerminalNode ARG() { return getToken(DaxParser.ARG, 0); }
		public ArgumentElPlainContext(ArgumentElContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterArgumentElPlain(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitArgumentElPlain(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitArgumentElPlain(this);
			else return visitor.visitChildren(this);
		}
	}
	public static class ArgumentElFilenameContext extends ArgumentElContext {
		public FilenameContext filename() {
			return getRuleContext(FilenameContext.class,0);
		}
		public ArgumentElFilenameContext(ArgumentElContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterArgumentElFilename(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitArgumentElFilename(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitArgumentElFilename(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgumentElContext argumentEl() throws RecognitionException {
		ArgumentElContext _localctx = new ArgumentElContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_argumentEl);
		try {
			setState(157);
			switch (_input.LA(1)) {
			case ARG:
				_localctx = new ArgumentElPlainContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(155); match(ARG);
				}
				break;
			case LTAG:
				_localctx = new ArgumentElFilenameContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(156); filename();
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

	public static class JobUsesPropContext extends ParserRuleContext {
		public TerminalNode FALSE() { return getToken(DaxParser.FALSE, 0); }
		public TerminalNode TRUE() { return getToken(DaxParser.TRUE, 0); }
		public TerminalNode TYPE() { return getToken(DaxParser.TYPE, 0); }
		public TerminalNode EXECUTABLE() { return getToken(DaxParser.EXECUTABLE, 0); }
		public TerminalNode FILE() { return getToken(DaxParser.FILE, 0); }
		public TerminalNode REGISTER() { return getToken(DaxParser.REGISTER, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public TerminalNode INPUT() { return getToken(DaxParser.INPUT, 0); }
		public TerminalNode OPTIONAL() { return getToken(DaxParser.OPTIONAL, 0); }
		public TerminalNode OUTPUT() { return getToken(DaxParser.OUTPUT, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode TRANSFER() { return getToken(DaxParser.TRANSFER, 0); }
		public TerminalNode LINK() { return getToken(DaxParser.LINK, 0); }
		public JobUsesPropContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_jobUsesProp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterJobUsesProp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitJobUsesProp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitJobUsesProp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final JobUsesPropContext jobUsesProp() throws RecognitionException {
		JobUsesPropContext _localctx = new JobUsesPropContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_jobUsesProp);
		int _la;
		try {
			setState(177);
			switch (_input.LA(1)) {
			case FILE:
				enterOuterAlt(_localctx, 1);
				{
				setState(159); match(FILE);
				setState(160); match(EQ);
				setState(161); match(STRING);
				}
				break;
			case LINK:
				enterOuterAlt(_localctx, 2);
				{
				setState(162); match(LINK);
				setState(163); match(EQ);
				setState(164);
				_la = _input.LA(1);
				if ( !(_la==INPUT || _la==OUTPUT) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				}
				break;
			case REGISTER:
				enterOuterAlt(_localctx, 3);
				{
				setState(165); match(REGISTER);
				setState(166); match(EQ);
				setState(167);
				_la = _input.LA(1);
				if ( !(_la==FALSE || _la==TRUE) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				}
				break;
			case TRANSFER:
				enterOuterAlt(_localctx, 4);
				{
				setState(168); match(TRANSFER);
				setState(169); match(EQ);
				setState(170);
				_la = _input.LA(1);
				if ( !(_la==FALSE || _la==TRUE) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				}
				break;
			case TYPE:
				enterOuterAlt(_localctx, 5);
				{
				setState(171); match(TYPE);
				setState(172); match(EQ);
				setState(173); match(EXECUTABLE);
				}
				break;
			case OPTIONAL:
				enterOuterAlt(_localctx, 6);
				{
				setState(174); match(OPTIONAL);
				setState(175); match(EQ);
				setState(176);
				_la = _input.LA(1);
				if ( !(_la==FALSE || _la==TRUE) ) {
				_errHandler.recoverInline(this);
				}
				consume();
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

	public static class ChildContext extends ParserRuleContext {
		public TerminalNode CHILD(int i) {
			return getToken(DaxParser.CHILD, i);
		}
		public List<TerminalNode> LTAG() { return getTokens(DaxParser.LTAG); }
		public List<ParentContext> parent() {
			return getRuleContexts(ParentContext.class);
		}
		public TerminalNode LTAG(int i) {
			return getToken(DaxParser.LTAG, i);
		}
		public List<TerminalNode> RTAG() { return getTokens(DaxParser.RTAG); }
		public TerminalNode SLASH() { return getToken(DaxParser.SLASH, 0); }
		public List<TerminalNode> CHILD() { return getTokens(DaxParser.CHILD); }
		public TerminalNode REF() { return getToken(DaxParser.REF, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public TerminalNode RTAG(int i) {
			return getToken(DaxParser.RTAG, i);
		}
		public ParentContext parent(int i) {
			return getRuleContext(ParentContext.class,i);
		}
		public ChildContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_child; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterChild(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitChild(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitChild(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ChildContext child() throws RecognitionException {
		ChildContext _localctx = new ChildContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_child);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(179); match(LTAG);
			setState(180); match(CHILD);
			setState(181); match(REF);
			setState(182); match(EQ);
			setState(183); match(STRING);
			setState(184); match(RTAG);
			setState(186); 
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(185); parent();
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(188); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			} while ( _alt!=2 && _alt!=-1 );
			setState(190); match(LTAG);
			setState(191); match(SLASH);
			setState(192); match(CHILD);
			setState(193); match(RTAG);
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

	public static class ParentContext extends ParserRuleContext {
		public TerminalNode PARENT() { return getToken(DaxParser.PARENT, 0); }
		public TerminalNode LTAG() { return getToken(DaxParser.LTAG, 0); }
		public TerminalNode SLASH() { return getToken(DaxParser.SLASH, 0); }
		public TerminalNode RTAG() { return getToken(DaxParser.RTAG, 0); }
		public TerminalNode REF() { return getToken(DaxParser.REF, 0); }
		public TerminalNode EQ() { return getToken(DaxParser.EQ, 0); }
		public TerminalNode STRING() { return getToken(DaxParser.STRING, 0); }
		public ParentContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parent; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).enterParent(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DaxListener ) ((DaxListener)listener).exitParent(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof DaxVisitor ) return ((DaxVisitor<? extends T>)visitor).visitParent(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ParentContext parent() throws RecognitionException {
		ParentContext _localctx = new ParentContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_parent);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(195); match(LTAG);
			setState(196); match(PARENT);
			setState(197); match(REF);
			setState(198); match(EQ);
			setState(199); match(STRING);
			setState(200); match(SLASH);
			setState(201); match(RTAG);
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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3,\u00ce\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\3\2\3\2\3\2\7\2 \n\2\f\2\16\2#\13\2\3\2"+
		"\3\2\7\2\'\n\2\f\2\16\2*\13\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3F"+
		"\n\3\3\4\3\4\3\4\5\4K\n\4\3\5\3\5\3\5\7\5P\n\5\f\5\16\5S\13\5\3\5\3\5"+
		"\3\5\3\6\3\6\3\6\3\6\3\6\3\6\5\6^\n\6\3\7\3\7\3\7\7\7c\n\7\f\7\16\7f\13"+
		"\7\3\7\3\7\7\7j\n\7\f\7\16\7m\13\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b"+
		"\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\b\u0086\n\b"+
		"\3\t\3\t\3\t\3\t\7\t\u008c\n\t\f\t\16\t\u008f\13\t\3\t\3\t\5\t\u0093\n"+
		"\t\3\n\3\n\7\n\u0097\n\n\f\n\16\n\u009a\13\n\3\n\3\n\3\13\3\13\5\13\u00a0"+
		"\n\13\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f"+
		"\3\f\3\f\5\f\u00b4\n\f\3\r\3\r\3\r\3\r\3\r\3\r\3\r\6\r\u00bd\n\r\r\r\16"+
		"\r\u00be\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3"+
		"\16\2\2\17\2\4\6\b\n\f\16\20\22\24\26\30\32\2\5\4\2\20\21\31\31\4\2\21"+
		"\21\31\31\4\2\13\13\"\"\u00dd\2\34\3\2\2\2\4E\3\2\2\2\6J\3\2\2\2\bL\3"+
		"\2\2\2\n]\3\2\2\2\f_\3\2\2\2\16\u0085\3\2\2\2\20\u0092\3\2\2\2\22\u0094"+
		"\3\2\2\2\24\u009f\3\2\2\2\26\u00b3\3\2\2\2\30\u00b5\3\2\2\2\32\u00c5\3"+
		"\2\2\2\34\35\7\26\2\2\35!\7\3\2\2\36 \5\4\3\2\37\36\3\2\2\2 #\3\2\2\2"+
		"!\37\3\2\2\2!\"\3\2\2\2\"$\3\2\2\2#!\3\2\2\2$(\7\36\2\2%\'\5\6\4\2&%\3"+
		"\2\2\2\'*\3\2\2\2(&\3\2\2\2()\3\2\2\2)+\3\2\2\2*(\3\2\2\2+,\7\26\2\2,"+
		"-\7 \2\2-.\7\3\2\2./\7\36\2\2/\3\3\2\2\2\60\61\7&\2\2\61\62\7\t\2\2\62"+
		"F\7(\2\2\63\64\7\'\2\2\64\65\7\t\2\2\65F\7(\2\2\66\67\7\37\2\2\678\7\t"+
		"\2\28F\7(\2\29:\7%\2\2:;\7\t\2\2;F\7(\2\2<=\7\6\2\2=>\7\t\2\2>F\7(\2\2"+
		"?@\7\17\2\2@A\7\t\2\2AF\7(\2\2BC\7\27\2\2CD\7\t\2\2DF\7(\2\2E\60\3\2\2"+
		"\2E\63\3\2\2\2E\66\3\2\2\2E9\3\2\2\2E<\3\2\2\2E?\3\2\2\2EB\3\2\2\2F\5"+
		"\3\2\2\2GK\5\b\5\2HK\5\f\7\2IK\5\30\r\2JG\3\2\2\2JH\3\2\2\2JI\3\2\2\2"+
		"K\7\3\2\2\2LM\7\26\2\2MQ\7\r\2\2NP\5\n\6\2ON\3\2\2\2PS\3\2\2\2QO\3\2\2"+
		"\2QR\3\2\2\2RT\3\2\2\2SQ\3\2\2\2TU\7 \2\2UV\7\36\2\2V\t\3\2\2\2WX\7\f"+
		"\2\2XY\7\t\2\2Y^\7(\2\2Z[\7\25\2\2[\\\7\t\2\2\\^\t\2\2\2]W\3\2\2\2]Z\3"+
		"\2\2\2^\13\3\2\2\2_`\7\26\2\2`d\7\22\2\2ac\5\16\b\2ba\3\2\2\2cf\3\2\2"+
		"\2db\3\2\2\2de\3\2\2\2eg\3\2\2\2fd\3\2\2\2gk\7\36\2\2hj\5\20\t\2ih\3\2"+
		"\2\2jm\3\2\2\2ki\3\2\2\2kl\3\2\2\2ln\3\2\2\2mk\3\2\2\2no\7\26\2\2op\7"+
		" \2\2pq\7\22\2\2qr\7\36\2\2r\r\3\2\2\2st\7\16\2\2tu\7\t\2\2u\u0086\7("+
		"\2\2vw\7\27\2\2wx\7\t\2\2x\u0086\7(\2\2yz\7%\2\2z{\7\t\2\2{\u0086\7(\2"+
		"\2|}\7\24\2\2}~\7\t\2\2~\u0086\7(\2\2\177\u0080\7\7\2\2\u0080\u0081\7"+
		"\t\2\2\u0081\u0086\7(\2\2\u0082\u0083\7\b\2\2\u0083\u0084\7\t\2\2\u0084"+
		"\u0086\7(\2\2\u0085s\3\2\2\2\u0085v\3\2\2\2\u0085y\3\2\2\2\u0085|\3\2"+
		"\2\2\u0085\177\3\2\2\2\u0085\u0082\3\2\2\2\u0086\17\3\2\2\2\u0087\u0093"+
		"\5\22\n\2\u0088\u0089\7\26\2\2\u0089\u008d\7$\2\2\u008a\u008c\5\26\f\2"+
		"\u008b\u008a\3\2\2\2\u008c\u008f\3\2\2\2\u008d\u008b\3\2\2\2\u008d\u008e"+
		"\3\2\2\2\u008e\u0090\3\2\2\2\u008f\u008d\3\2\2\2\u0090\u0091\7 \2\2\u0091"+
		"\u0093\7\36\2\2\u0092\u0087\3\2\2\2\u0092\u0088\3\2\2\2\u0093\21\3\2\2"+
		"\2\u0094\u0098\7\23\2\2\u0095\u0097\5\24\13\2\u0096\u0095\3\2\2\2\u0097"+
		"\u009a\3\2\2\2\u0098\u0096\3\2\2\2\u0098\u0099\3\2\2\2\u0099\u009b\3\2"+
		"\2\2\u009a\u0098\3\2\2\2\u009b\u009c\7\33\2\2\u009c\23\3\2\2\2\u009d\u00a0"+
		"\7)\2\2\u009e\u00a0\5\b\5\2\u009f\u009d\3\2\2\2\u009f\u009e\3\2\2\2\u00a0"+
		"\25\3\2\2\2\u00a1\u00a2\7\f\2\2\u00a2\u00a3\7\t\2\2\u00a3\u00b4\7(\2\2"+
		"\u00a4\u00a5\7\25\2\2\u00a5\u00a6\7\t\2\2\u00a6\u00b4\t\3\2\2\u00a7\u00a8"+
		"\7\35\2\2\u00a8\u00a9\7\t\2\2\u00a9\u00b4\t\4\2\2\u00aa\u00ab\7!\2\2\u00ab"+
		"\u00ac\7\t\2\2\u00ac\u00b4\t\4\2\2\u00ad\u00ae\7#\2\2\u00ae\u00af\7\t"+
		"\2\2\u00af\u00b4\7\n\2\2\u00b0\u00b1\7\30\2\2\u00b1\u00b2\7\t\2\2\u00b2"+
		"\u00b4\t\4\2\2\u00b3\u00a1\3\2\2\2\u00b3\u00a4\3\2\2\2\u00b3\u00a7\3\2"+
		"\2\2\u00b3\u00aa\3\2\2\2\u00b3\u00ad\3\2\2\2\u00b3\u00b0\3\2\2\2\u00b4"+
		"\27\3\2\2\2\u00b5\u00b6\7\26\2\2\u00b6\u00b7\7\5\2\2\u00b7\u00b8\7\34"+
		"\2\2\u00b8\u00b9\7\t\2\2\u00b9\u00ba\7(\2\2\u00ba\u00bc\7\36\2\2\u00bb"+
		"\u00bd\5\32\16\2\u00bc\u00bb\3\2\2\2\u00bd\u00be\3\2\2\2\u00be\u00bc\3"+
		"\2\2\2\u00be\u00bf\3\2\2\2\u00bf\u00c0\3\2\2\2\u00c0\u00c1\7\26\2\2\u00c1"+
		"\u00c2\7 \2\2\u00c2\u00c3\7\5\2\2\u00c3\u00c4\7\36\2\2\u00c4\31\3\2\2"+
		"\2\u00c5\u00c6\7\26\2\2\u00c6\u00c7\7\32\2\2\u00c7\u00c8\7\34\2\2\u00c8"+
		"\u00c9\7\t\2\2\u00c9\u00ca\7(\2\2\u00ca\u00cb\7 \2\2\u00cb\u00cc\7\36"+
		"\2\2\u00cc\33\3\2\2\2\21!(EJQ]dk\u0085\u008d\u0092\u0098\u009f\u00b3\u00be";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}