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

package de.huberlin.cuneiform.dax.repl;

import java.util.UUID;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import de.huberlin.cuneiform.dax.semanticmodel.DaxSemanticModelListener;
import de.huberlin.cuneiform.libdax.parser.DaxLexer;
import de.huberlin.cuneiform.libdax.parser.DaxParser;
import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.TopLevelContext;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;

public class DaxRepl extends BaseRepl {

	public DaxRepl( TicketSrcActor ticketSrc ) {
		super( ticketSrc );
	}
	
	@Override
	public synchronized int interpret( String input ) throws CloneNotSupportedException {
		
		DaxSemanticModelListener adag;
		TopLevelContext tlc;
		
		adag = process( input );
		tlc = adag.toTopLevelContext();
		
		return interpret( tlc );
		
	}

	@Override
	public void queryFailedPost( UUID queryId, Long ticketId, Exception e,
			String script, String stdOut, String stdErr ) {}

	@Override
	public void queryFinishedPost( UUID queryId, CompoundExpr result ) {}

	@Override
	public void queryStartedPost( UUID runId ) {}

	public static DaxSemanticModelListener process( String input ) {
		
		ANTLRInputStream instream;
		DaxLexer lexer;
		CommonTokenStream tokenStream;
		DaxParser parser;
		ParseTree tree;
		ParseTreeWalker walker;
		DaxSemanticModelListener adag;
		
		walker = new ParseTreeWalker();
		
		// parse original content
		instream = new ANTLRInputStream( input );
		
		lexer = new DaxLexer( instream );
		lexer.removeErrorListeners();

		tokenStream = new CommonTokenStream( lexer );
		
		parser = new DaxParser( tokenStream );
		parser.removeErrorListeners();
		
		adag = new DaxSemanticModelListener();
		lexer.addErrorListener( adag );
		parser.addErrorListener( adag );

		
		tree = parser.adag();
		
		walker.walk( adag, tree );
		
		return adag;
	}
	

}
