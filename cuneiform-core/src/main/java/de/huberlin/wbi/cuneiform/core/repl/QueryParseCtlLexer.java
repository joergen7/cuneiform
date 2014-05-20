package de.huberlin.wbi.cuneiform.core.repl;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

import de.huberlin.wbi.cuneiform.core.parser.ParseCtlLexer;

public class QueryParseCtlLexer extends ParseCtlLexer {
	
	public QueryParseCtlLexer( CharStream input ) {
		super( input );
	}

	public boolean isReady() {
		
		int depth;
		Token t;
		boolean ready;
		
		reset();
		
		depth = 0;
		ready = true;
		while( ( t = nextToken() ).getType() != Recognizer.EOF ) {
		
			switch( t.getType() ) {
			
				case ParseCtlLexer.LXCOMMENT :
				case ParseCtlLexer.LCCOMMENT :
				case ParseCtlLexer.LBRACE    : depth++; break;

				case ParseCtlLexer.RXCOMMENT :
				case ParseCtlLexer.RCCOMMENT :
				case ParseCtlLexer.RBRACE    : depth--; if( depth == 0 ) ready = true; break;

				case ParseCtlLexer.RMMECB    : if( depth == 0 ) ready = true; break; 
				case ParseCtlLexer.ANY       : if( depth == 0 ) ready = false; break;
				case ParseCtlLexer.SEMICOLON : if( depth == 0 ) ready = true; break;
				default : // ignore
			}
		}
		
		if( depth != 0 )
			return false;
		
		return ready;
	}
}
