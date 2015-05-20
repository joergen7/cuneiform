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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.UUID;

import org.apache.commons.logging.Log;

import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.ticketsrc.ReplTicketSrc;

public class InteractiveRepl extends BaseRepl {
	
	public InteractiveRepl( ReplTicketSrc ticketSrc, Log statLog ) {
		super( ticketSrc, statLog );
	}

	@Override
	public synchronized void queryFinishedPost( UUID queryId, CompoundExpr result ) {
		// nothing to do
	}

	@Override
	public synchronized void queryStartedPost( UUID runId ) {
		// nothing to do
	}
	
	@Override
	public synchronized void queryFailedPost( UUID queryId, Long ticketId, Exception e, String script, String stdOut, String stdErr ) {

		String line;
		int i;
		
		if( script != null )
			try( BufferedReader reader = new BufferedReader( new StringReader( script ) ) ) {
				
				System.out.println( "[script]" );
				
				i = 0;
	
				while( ( line = reader.readLine() ) != null )
					System.out.println( String.format( "%3d  %s", ++i, line ) );
	
			}
			catch( IOException e1 ) {
				throw new RuntimeException( e1 );
			}
		
		if( stdOut != null ) {
			System.out.println( "[out]" );
			System.out.println( stdOut );
		}
		
		if( stdErr != null ) {
			System.out.println( "[err]" );
			System.out.println( stdErr );
		}
		
		if( e != null ) {
			System.out.println( "[trace]" );
			e.printStackTrace();
		}
	
	}
}
