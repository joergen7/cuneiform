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
