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

package de.huberlin.wbi.cuneiform.core.main;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.antlr.v4.runtime.ANTLRInputStream;

import de.huberlin.wbi.cuneiform.core.cre.LocalCreActor;
import de.huberlin.wbi.cuneiform.core.preprocess.ParseException;
import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;
import de.huberlin.wbi.cuneiform.core.repl.QueryParseCtlLexer;
import de.huberlin.wbi.cuneiform.core.repl.CmdlineRepl;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;



public class Main {

	public static void main( String[] args ) throws IOException {
		
		TicketSrcActor ticketSrc;
		CmdlineRepl repl;
		File sandbox;
		LocalCreActor creActor;
		ExecutorService executor;
		StringBuffer buf;
		String line;
		QueryParseCtlLexer lexer;
		int ctl;
		
		System.out.println( BaseRepl.getLogo() );
		
		sandbox = new File( "/tmp/cuneiform" );
		sandbox.mkdir();
		
		executor = Executors.newCachedThreadPool();
		
		creActor = new LocalCreActor( sandbox );
		executor.submit( creActor );
		
		ticketSrc = new TicketSrcActor( creActor );
		executor.submit( ticketSrc );
		executor.shutdown();
		
		
		repl = new CmdlineRepl( ticketSrc );		
		buf = new StringBuffer();
		System.out.print( "> " );
		
		try( BufferedReader reader = new BufferedReader( new InputStreamReader( System.in ) ) ) {
			
			while( ( line = reader.readLine() ) != null ) {
				
				buf.append( line ).append( '\n' );
				
				lexer = new QueryParseCtlLexer( new ANTLRInputStream( buf.toString() ) );
				
				
				if( lexer.isReady() ) {
					
					try {
						
						ctl = repl.interpret( buf.toString() );
						
						if( ( ctl & BaseRepl.CTL_STATE ) > 0 )
							System.out.print( repl.getState() );
						
						if( ( ctl & BaseRepl.CTL_QUERYSET ) > 0 )
							System.out.print( repl.getRunningSet() );
						
						if( ( ctl & BaseRepl.CTL_TICKETSET ) > 0 )
							System.out.print( repl.getTicketSet() );

						if( ( ctl & BaseRepl.CTL_QUIT ) > 0 )
							break;
					}
					catch( ParseException e ) {
						System.out.println( e );
					}
					
					buf = new StringBuffer();	
					System.out.print( "> " );
				}
				
			}
			
		}
		System.out.println( "Bye." );
		executor.shutdownNow();		
	}
}
