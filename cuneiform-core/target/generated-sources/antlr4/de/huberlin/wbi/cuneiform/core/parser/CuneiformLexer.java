// Generated from de/huberlin/wbi/cuneiform/core/parser/Cuneiform.g4 by ANTLR 4.2
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
public class CuneiformLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		APPLY=1, COLON=2, COMB=3, COMBR=4, CURRY=5, DEFTASK=6, EQUAL=7, ELSE=8, 
		FROMSTACK=9, IF=10, IN=11, IMPORT=12, INCLUDE=13, LAMBDA=14, LBRACE=15, 
		LPAREN=16, LSQUAREBR=17, LTAG=18, NIL=19, PERM=20, RBRACE=21, RPAREN=22, 
		RSQUAREBR=23, RTAG=24, SEMICOLON=25, TILDE=26, THEN=27, TOSTACK=28, VAR=29, 
		INT=30, BODY=31, OPENBODY=32, STRING=33, COMMENT=34, ID=35, WS=36;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'apply'", "':'", "'comb'", "'combr'", "'curry'", "'deftask'", "'='", 
		"'else'", "FROMSTACK", "'if'", "'in'", "'import'", "'include'", "'\\'", 
		"'{'", "'('", "'['", "'<'", "'nil'", "'perm'", "'}'", "')'", "']'", "'>'", 
		"';'", "'~'", "'then'", "TOSTACK", "'var'", "INT", "BODY", "OPENBODY", 
		"STRING", "COMMENT", "ID", "WS"
	};
	public static final String[] ruleNames = {
		"APPLY", "COLON", "COMB", "COMBR", "CURRY", "DEFTASK", "EQUAL", "ELSE", 
		"FROMSTACK", "IF", "IN", "IMPORT", "INCLUDE", "LAMBDA", "LBRACE", "LPAREN", 
		"LSQUAREBR", "LTAG", "NIL", "PERM", "RBRACE", "RPAREN", "RSQUAREBR", "RTAG", 
		"SEMICOLON", "TILDE", "THEN", "TOSTACK", "VAR", "INT", "BODY", "OPENBODY", 
		"LMMECB", "RMMECB", "STRING", "COMMENT", "ID", "WS"
	};


	public CuneiformLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Cuneiform.g4"; }

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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2&\u0133\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\3\2\3\2\3\2\3\2\3\2\3\2\3"+
		"\3\3\3\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6"+
		"\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\n\3"+
		"\n\6\nz\n\n\r\n\16\n{\3\n\3\n\3\13\3\13\3\13\3\f\3\f\3\f\3\r\3\r\3\r\3"+
		"\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\17\3\17\3\20"+
		"\3\20\3\21\3\21\3\22\3\22\3\23\3\23\3\24\3\24\3\24\3\24\3\25\3\25\3\25"+
		"\3\25\3\25\3\26\3\26\3\27\3\27\3\30\3\30\3\31\3\31\3\32\3\32\3\33\3\33"+
		"\3\34\3\34\3\34\3\34\3\34\3\35\6\35\u00ba\n\35\r\35\16\35\u00bb\3\35\3"+
		"\35\3\36\3\36\3\36\3\36\3\37\3\37\5\37\u00c6\n\37\3\37\3\37\7\37\u00ca"+
		"\n\37\f\37\16\37\u00cd\13\37\5\37\u00cf\n\37\3 \3 \7 \u00d3\n \f \16 "+
		"\u00d6\13 \3 \3 \3!\3!\7!\u00dc\n!\f!\16!\u00df\13!\3\"\3\"\3\"\3#\3#"+
		"\3#\3$\3$\3$\3$\3$\3$\7$\u00ed\n$\f$\16$\u00f0\13$\3$\3$\3$\3$\3$\3$\3"+
		"$\7$\u00f9\n$\f$\16$\u00fc\13$\3$\5$\u00ff\n$\3%\3%\3%\3%\5%\u0105\n%"+
		"\3%\7%\u0108\n%\f%\16%\u010b\13%\3%\3%\3%\3%\7%\u0111\n%\f%\16%\u0114"+
		"\13%\3%\3%\3%\3%\3%\3%\3%\3%\7%\u011e\n%\f%\16%\u0121\13%\3%\3%\3%\3%"+
		"\5%\u0127\n%\3%\3%\3&\6&\u012c\n&\r&\16&\u012d\3\'\3\'\3\'\3\'\b\u00d4"+
		"\u00dd\u00ee\u00fa\u0112\u011f\2(\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23"+
		"\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31"+
		"\61\32\63\33\65\34\67\359\36;\37= ?!A\"C\2E\2G#I$K%M&\3\2\7\3\2\62;\3"+
		"\2\63;\3\2\f\f\b\2,-/;C\\^^aac|\6\2\13\f\17\17\"\"..\u0147\2\3\3\2\2\2"+
		"\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2"+
		"\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2"+
		"\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2"+
		"\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2"+
		"\2\2\63\3\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2"+
		"\2\2\2?\3\2\2\2\2A\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2\2K\3\2\2\2\2M\3\2\2\2"+
		"\3O\3\2\2\2\5U\3\2\2\2\7W\3\2\2\2\t\\\3\2\2\2\13b\3\2\2\2\rh\3\2\2\2\17"+
		"p\3\2\2\2\21r\3\2\2\2\23w\3\2\2\2\25\177\3\2\2\2\27\u0082\3\2\2\2\31\u0085"+
		"\3\2\2\2\33\u008c\3\2\2\2\35\u0094\3\2\2\2\37\u0096\3\2\2\2!\u0098\3\2"+
		"\2\2#\u009a\3\2\2\2%\u009c\3\2\2\2\'\u009e\3\2\2\2)\u00a2\3\2\2\2+\u00a7"+
		"\3\2\2\2-\u00a9\3\2\2\2/\u00ab\3\2\2\2\61\u00ad\3\2\2\2\63\u00af\3\2\2"+
		"\2\65\u00b1\3\2\2\2\67\u00b3\3\2\2\29\u00b9\3\2\2\2;\u00bf\3\2\2\2=\u00ce"+
		"\3\2\2\2?\u00d0\3\2\2\2A\u00d9\3\2\2\2C\u00e0\3\2\2\2E\u00e3\3\2\2\2G"+
		"\u00fe\3\2\2\2I\u0126\3\2\2\2K\u012b\3\2\2\2M\u012f\3\2\2\2OP\7c\2\2P"+
		"Q\7r\2\2QR\7r\2\2RS\7n\2\2ST\7{\2\2T\4\3\2\2\2UV\7<\2\2V\6\3\2\2\2WX\7"+
		"e\2\2XY\7q\2\2YZ\7o\2\2Z[\7d\2\2[\b\3\2\2\2\\]\7e\2\2]^\7q\2\2^_\7o\2"+
		"\2_`\7d\2\2`a\7t\2\2a\n\3\2\2\2bc\7e\2\2cd\7w\2\2de\7t\2\2ef\7t\2\2fg"+
		"\7{\2\2g\f\3\2\2\2hi\7f\2\2ij\7g\2\2jk\7h\2\2kl\7v\2\2lm\7c\2\2mn\7u\2"+
		"\2no\7m\2\2o\16\3\2\2\2pq\7?\2\2q\20\3\2\2\2rs\7g\2\2st\7n\2\2tu\7u\2"+
		"\2uv\7g\2\2v\22\3\2\2\2wy\7>\2\2xz\7/\2\2yx\3\2\2\2z{\3\2\2\2{y\3\2\2"+
		"\2{|\3\2\2\2|}\3\2\2\2}~\7-\2\2~\24\3\2\2\2\177\u0080\7k\2\2\u0080\u0081"+
		"\7h\2\2\u0081\26\3\2\2\2\u0082\u0083\7k\2\2\u0083\u0084\7p\2\2\u0084\30"+
		"\3\2\2\2\u0085\u0086\7k\2\2\u0086\u0087\7o\2\2\u0087\u0088\7r\2\2\u0088"+
		"\u0089\7q\2\2\u0089\u008a\7t\2\2\u008a\u008b\7v\2\2\u008b\32\3\2\2\2\u008c"+
		"\u008d\7k\2\2\u008d\u008e\7p\2\2\u008e\u008f\7e\2\2\u008f\u0090\7n\2\2"+
		"\u0090\u0091\7w\2\2\u0091\u0092\7f\2\2\u0092\u0093\7g\2\2\u0093\34\3\2"+
		"\2\2\u0094\u0095\7^\2\2\u0095\36\3\2\2\2\u0096\u0097\7}\2\2\u0097 \3\2"+
		"\2\2\u0098\u0099\7*\2\2\u0099\"\3\2\2\2\u009a\u009b\7]\2\2\u009b$\3\2"+
		"\2\2\u009c\u009d\7>\2\2\u009d&\3\2\2\2\u009e\u009f\7p\2\2\u009f\u00a0"+
		"\7k\2\2\u00a0\u00a1\7n\2\2\u00a1(\3\2\2\2\u00a2\u00a3\7r\2\2\u00a3\u00a4"+
		"\7g\2\2\u00a4\u00a5\7t\2\2\u00a5\u00a6\7o\2\2\u00a6*\3\2\2\2\u00a7\u00a8"+
		"\7\177\2\2\u00a8,\3\2\2\2\u00a9\u00aa\7+\2\2\u00aa.\3\2\2\2\u00ab\u00ac"+
		"\7_\2\2\u00ac\60\3\2\2\2\u00ad\u00ae\7@\2\2\u00ae\62\3\2\2\2\u00af\u00b0"+
		"\7=\2\2\u00b0\64\3\2\2\2\u00b1\u00b2\7\u0080\2\2\u00b2\66\3\2\2\2\u00b3"+
		"\u00b4\7v\2\2\u00b4\u00b5\7j\2\2\u00b5\u00b6\7g\2\2\u00b6\u00b7\7p\2\2"+
		"\u00b78\3\2\2\2\u00b8\u00ba\7/\2\2\u00b9\u00b8\3\2\2\2\u00ba\u00bb\3\2"+
		"\2\2\u00bb\u00b9\3\2\2\2\u00bb\u00bc\3\2\2\2\u00bc\u00bd\3\2\2\2\u00bd"+
		"\u00be\7-\2\2\u00be:\3\2\2\2\u00bf\u00c0\7x\2\2\u00c0\u00c1\7c\2\2\u00c1"+
		"\u00c2\7t\2\2\u00c2<\3\2\2\2\u00c3\u00cf\t\2\2\2\u00c4\u00c6\7/\2\2\u00c5"+
		"\u00c4\3\2\2\2\u00c5\u00c6\3\2\2\2\u00c6\u00c7\3\2\2\2\u00c7\u00cb\t\3"+
		"\2\2\u00c8\u00ca\t\2\2\2\u00c9\u00c8\3\2\2\2\u00ca\u00cd\3\2\2\2\u00cb"+
		"\u00c9\3\2\2\2\u00cb\u00cc\3\2\2\2\u00cc\u00cf\3\2\2\2\u00cd\u00cb\3\2"+
		"\2\2\u00ce\u00c3\3\2\2\2\u00ce\u00c5\3\2\2\2\u00cf>\3\2\2\2\u00d0\u00d4"+
		"\5C\"\2\u00d1\u00d3\13\2\2\2\u00d2\u00d1\3\2\2\2\u00d3\u00d6\3\2\2\2\u00d4"+
		"\u00d5\3\2\2\2\u00d4\u00d2\3\2\2\2\u00d5\u00d7\3\2\2\2\u00d6\u00d4\3\2"+
		"\2\2\u00d7\u00d8\5E#\2\u00d8@\3\2\2\2\u00d9\u00dd\5C\"\2\u00da\u00dc\13"+
		"\2\2\2\u00db\u00da\3\2\2\2\u00dc\u00df\3\2\2\2\u00dd\u00de\3\2\2\2\u00dd"+
		"\u00db\3\2\2\2\u00deB\3\2\2\2\u00df\u00dd\3\2\2\2\u00e0\u00e1\7,\2\2\u00e1"+
		"\u00e2\7}\2\2\u00e2D\3\2\2\2\u00e3\u00e4\7\177\2\2\u00e4\u00e5\7,\2\2"+
		"\u00e5F\3\2\2\2\u00e6\u00ee\7)\2\2\u00e7\u00e8\7^\2\2\u00e8\u00ed\7)\2"+
		"\2\u00e9\u00ea\7^\2\2\u00ea\u00ed\7^\2\2\u00eb\u00ed\13\2\2\2\u00ec\u00e7"+
		"\3\2\2\2\u00ec\u00e9\3\2\2\2\u00ec\u00eb\3\2\2\2\u00ed\u00f0\3\2\2\2\u00ee"+
		"\u00ef\3\2\2\2\u00ee\u00ec\3\2\2\2\u00ef\u00f1\3\2\2\2\u00f0\u00ee\3\2"+
		"\2\2\u00f1\u00ff\7)\2\2\u00f2\u00fa\7$\2\2\u00f3\u00f4\7^\2\2\u00f4\u00f9"+
		"\7$\2\2\u00f5\u00f6\7^\2\2\u00f6\u00f9\7^\2\2\u00f7\u00f9\13\2\2\2\u00f8"+
		"\u00f3\3\2\2\2\u00f8\u00f5\3\2\2\2\u00f8\u00f7\3\2\2\2\u00f9\u00fc\3\2"+
		"\2\2\u00fa\u00fb\3\2\2\2\u00fa\u00f8\3\2\2\2\u00fb\u00fd\3\2\2\2\u00fc"+
		"\u00fa\3\2\2\2\u00fd\u00ff\7$\2\2\u00fe\u00e6\3\2\2\2\u00fe\u00f2\3\2"+
		"\2\2\u00ffH\3\2\2\2\u0100\u0105\7%\2\2\u0101\u0102\7\61\2\2\u0102\u0105"+
		"\7\61\2\2\u0103\u0105\7\'\2\2\u0104\u0100\3\2\2\2\u0104\u0101\3\2\2\2"+
		"\u0104\u0103\3\2\2\2\u0105\u0109\3\2\2\2\u0106\u0108\n\4\2\2\u0107\u0106"+
		"\3\2\2\2\u0108\u010b\3\2\2\2\u0109\u0107\3\2\2\2\u0109\u010a\3\2\2\2\u010a"+
		"\u0127\3\2\2\2\u010b\u0109\3\2\2\2\u010c\u010d\7\61\2\2\u010d\u010e\7"+
		",\2\2\u010e\u0112\3\2\2\2\u010f\u0111\13\2\2\2\u0110\u010f\3\2\2\2\u0111"+
		"\u0114\3\2\2\2\u0112\u0113\3\2\2\2\u0112\u0110\3\2\2\2\u0113\u0115\3\2"+
		"\2\2\u0114\u0112\3\2\2\2\u0115\u0116\7,\2\2\u0116\u0127\7\61\2\2\u0117"+
		"\u0118\7>\2\2\u0118\u0119\7#\2\2\u0119\u011a\7/\2\2\u011a\u011b\7/\2\2"+
		"\u011b\u011f\3\2\2\2\u011c\u011e\13\2\2\2\u011d\u011c\3\2\2\2\u011e\u0121"+
		"\3\2\2\2\u011f\u0120\3\2\2\2\u011f\u011d\3\2\2\2\u0120\u0122\3\2\2\2\u0121"+
		"\u011f\3\2\2\2\u0122\u0123\7/\2\2\u0123\u0124\7/\2\2\u0124\u0127\7@\2"+
		"\2\u0125\u0127\7~\2\2\u0126\u0104\3\2\2\2\u0126\u010c\3\2\2\2\u0126\u0117"+
		"\3\2\2\2\u0126\u0125\3\2\2\2\u0127\u0128\3\2\2\2\u0128\u0129\b%\2\2\u0129"+
		"J\3\2\2\2\u012a\u012c\t\5\2\2\u012b\u012a\3\2\2\2\u012c\u012d\3\2\2\2"+
		"\u012d\u012b\3\2\2\2\u012d\u012e\3\2\2\2\u012eL\3\2\2\2\u012f\u0130\t"+
		"\6\2\2\u0130\u0131\3\2\2\2\u0131\u0132\b\'\3\2\u0132N\3\2\2\2\25\2{\u00bb"+
		"\u00c5\u00cb\u00ce\u00d4\u00dd\u00ec\u00ee\u00f8\u00fa\u00fe\u0104\u0109"+
		"\u0112\u011f\u0126\u012d\4\b\2\2\2\3\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}