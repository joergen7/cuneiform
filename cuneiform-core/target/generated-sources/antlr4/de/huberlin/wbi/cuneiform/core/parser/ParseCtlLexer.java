// Generated from de/huberlin/wbi/cuneiform/core/parser/ParseCtlLexer.g4 by ANTLR 4.2
package de.huberlin.wbi.cuneiform.core.parser;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ParseCtlLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		RC=1, OC=2, CC=3, LCCOMMENT=4, LPAREN=5, LXCOMMENT=6, LMMECB=7, LBRACE=8, 
		RBRACE=9, SEMICOLON=10, WS=11, ANY=12, NEWLINE=13, ANYC=14, RCCOMMENT=15, 
		ANYCC=16, RXCOMMENT=17, ANYXC=18, RPAREN=19, ANYA=20, RMMECB=21, ANYF=22;
	public static final int COMMENT = 1;
	public static final int CCOMMENT = 2;
	public static final int XCOMMENT = 3;
	public static final int APPLY = 4;
	public static final int FOREIGN = 5;
	public static String[] modeNames = {
		"DEFAULT_MODE", "COMMENT", "CCOMMENT", "XCOMMENT", "APPLY", "FOREIGN"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'#'", "'%'", "'//'", "'/*'", "'('", "'<!--'", "'*{'", "'{'", "'}'", "';'", 
		"WS", "ANY", "NEWLINE", "ANYC", "'*/'", "ANYCC", "'-->'", "ANYXC", "')'", 
		"ANYA", "'}*'", "ANYF"
	};
	public static final String[] ruleNames = {
		"RC", "OC", "CC", "LCCOMMENT", "LPAREN", "LXCOMMENT", "LMMECB", "LBRACE", 
		"RBRACE", "SEMICOLON", "WS", "ANY", "NEWLINE", "ANYC", "RCCOMMENT", "ANYCC", 
		"RXCOMMENT", "ANYXC", "RPAREN", "ANYA", "RMMECB", "ANYF"
	};


	public ParseCtlLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "ParseCtlLexer.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\30\u008e\b\1\b\1"+
		"\b\1\b\1\b\1\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t"+
		"\b\4\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20"+
		"\t\20\4\21\t\21\4\22\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27"+
		"\t\27\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5"+
		"\3\5\3\5\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3"+
		"\b\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\f\3\f\3\r\3\r\3\16\3\16\3\16\3"+
		"\16\3\17\3\17\3\17\3\17\3\20\3\20\3\20\3\20\3\20\3\21\3\21\3\21\3\21\3"+
		"\22\3\22\3\22\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\24\3\24\3\24\3\24\3"+
		"\25\3\25\3\25\3\25\3\26\3\26\3\26\3\26\3\26\3\27\3\27\3\27\3\27\2\2\30"+
		"\b\3\n\4\f\5\16\6\20\7\22\b\24\t\26\n\30\13\32\f\34\r\36\16 \17\"\20$"+
		"\21&\22(\23*\24,\25.\26\60\27\62\30\b\2\3\4\5\6\7\4\6\2\13\f\17\17\"\""+
		"~~\4\2\f\f\17\17\u0088\2\b\3\2\2\2\2\n\3\2\2\2\2\f\3\2\2\2\2\16\3\2\2"+
		"\2\2\20\3\2\2\2\2\22\3\2\2\2\2\24\3\2\2\2\2\26\3\2\2\2\2\30\3\2\2\2\2"+
		"\32\3\2\2\2\2\34\3\2\2\2\2\36\3\2\2\2\3 \3\2\2\2\3\"\3\2\2\2\4$\3\2\2"+
		"\2\4&\3\2\2\2\5(\3\2\2\2\5*\3\2\2\2\6,\3\2\2\2\6.\3\2\2\2\7\60\3\2\2\2"+
		"\7\62\3\2\2\2\b\64\3\2\2\2\n8\3\2\2\2\f<\3\2\2\2\16A\3\2\2\2\20F\3\2\2"+
		"\2\22J\3\2\2\2\24Q\3\2\2\2\26V\3\2\2\2\30X\3\2\2\2\32Z\3\2\2\2\34\\\3"+
		"\2\2\2\36`\3\2\2\2 b\3\2\2\2\"f\3\2\2\2$j\3\2\2\2&o\3\2\2\2(s\3\2\2\2"+
		"*y\3\2\2\2,}\3\2\2\2.\u0081\3\2\2\2\60\u0085\3\2\2\2\62\u008a\3\2\2\2"+
		"\64\65\7%\2\2\65\66\3\2\2\2\66\67\b\2\2\2\67\t\3\2\2\289\7\'\2\29:\3\2"+
		"\2\2:;\b\3\2\2;\13\3\2\2\2<=\7\61\2\2=>\7\61\2\2>?\3\2\2\2?@\b\4\2\2@"+
		"\r\3\2\2\2AB\7\61\2\2BC\7,\2\2CD\3\2\2\2DE\b\5\3\2E\17\3\2\2\2FG\7*\2"+
		"\2GH\3\2\2\2HI\b\6\4\2I\21\3\2\2\2JK\7>\2\2KL\7#\2\2LM\7/\2\2MN\7/\2\2"+
		"NO\3\2\2\2OP\b\7\5\2P\23\3\2\2\2QR\7,\2\2RS\7}\2\2ST\3\2\2\2TU\b\b\6\2"+
		"U\25\3\2\2\2VW\7}\2\2W\27\3\2\2\2XY\7\177\2\2Y\31\3\2\2\2Z[\7=\2\2[\33"+
		"\3\2\2\2\\]\t\2\2\2]^\3\2\2\2^_\b\f\7\2_\35\3\2\2\2`a\13\2\2\2a\37\3\2"+
		"\2\2bc\t\3\2\2cd\3\2\2\2de\b\16\b\2e!\3\2\2\2fg\13\2\2\2gh\3\2\2\2hi\b"+
		"\17\7\2i#\3\2\2\2jk\7,\2\2kl\7\61\2\2lm\3\2\2\2mn\b\20\b\2n%\3\2\2\2o"+
		"p\13\2\2\2pq\3\2\2\2qr\b\21\7\2r\'\3\2\2\2st\7/\2\2tu\7/\2\2uv\7@\2\2"+
		"vw\3\2\2\2wx\b\22\b\2x)\3\2\2\2yz\13\2\2\2z{\3\2\2\2{|\b\23\7\2|+\3\2"+
		"\2\2}~\7+\2\2~\177\3\2\2\2\177\u0080\b\24\b\2\u0080-\3\2\2\2\u0081\u0082"+
		"\13\2\2\2\u0082\u0083\3\2\2\2\u0083\u0084\b\25\7\2\u0084/\3\2\2\2\u0085"+
		"\u0086\7\177\2\2\u0086\u0087\7,\2\2\u0087\u0088\3\2\2\2\u0088\u0089\b"+
		"\26\b\2\u0089\61\3\2\2\2\u008a\u008b\13\2\2\2\u008b\u008c\3\2\2\2\u008c"+
		"\u008d\b\27\7\2\u008d\63\3\2\2\2\b\2\3\4\5\6\7\t\4\3\2\4\4\2\4\6\2\4\5"+
		"\2\4\7\2\b\2\2\4\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}