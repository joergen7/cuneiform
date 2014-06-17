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

package de.huberlin.wbi.cuneiform.core.ticketsrc;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import de.huberlin.wbi.cuneiform.core.actormodel.Actor;
import de.huberlin.wbi.cuneiform.core.actormodel.Message;
import de.huberlin.wbi.cuneiform.core.cre.BaseCreActor;
import de.huberlin.wbi.cuneiform.core.cre.TicketReadyMsg;
import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ForeignLambdaExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NameExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotBoundException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Prototype;
import de.huberlin.wbi.cuneiform.core.semanticmodel.QualifiedTicket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ReduceVar;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CfSemanticModelVisitor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class TicketSrcActor extends Actor implements ReplTicketSrc {
	
	private final Set<BaseRepl> replSet;
	private final Map<UUID,Set<Ticket>> queryTicketMap;
	private final Map<Ticket,Set<UUID>> ticketQueryMap;
	private final Map<Long,Ticket> cacheMap;
	private final UUID runId;
	private final BaseCreActor cre;
	private final Set<UUID> failedQuerySet;
	
	
	public TicketSrcActor( BaseCreActor cre ) {
		if( cre == null )
			throw new NullPointerException( "CRE actor must not be null." );
		
		this.cre = cre;

		queryTicketMap = new HashMap<>();
		ticketQueryMap = new HashMap<>();
		cacheMap = new HashMap<>();
		replSet = new HashSet<>();
		runId = UUID.randomUUID();
		failedQuerySet = new HashSet<>();

		if( log.isDebugEnabled() )
			log.debug( "New TicketSrcActor's UUID: "+runId );
	}
	
	public UUID getRunId() {
		return runId;
	}
	
	@Override
	public synchronized Set<Ticket> getTicketSet( UUID queryId ) {
		return queryTicketMap.get( queryId );
	}
		
	@Override
	public synchronized boolean isQueueClear( UUID queryId ) {
		
		Set<Ticket> set;
		
		if( queryId == null )
			throw new NullPointerException( "Run ID must not be null." );
		
		set = queryTicketMap.get( queryId );

		if( set == null )
			return true;
		
		return set.isEmpty();		
	}
	
	@Override
	protected void processMsg( Message msg ) {

		TicketFinishedMsg ticketFinishedMsg;
		TicketFailedMsg ticketFailedMsg;
		Ticket ticket;
		String script, stdOut, stdErr;
		long ticketId;
		Exception e;
		Set<UUID> querySet;
		

		if( msg instanceof TicketFinishedMsg ) {
			
			ticketFinishedMsg = ( TicketFinishedMsg )msg;
			ticket = ticketFinishedMsg.getTicket();
			ticketId = ticket.getTicketId();
			querySet = ticketQueryMap.get( ticket );

			clearTicket( ticket );

			if( querySet != null )
				for( UUID queryId : querySet )				
					for( BaseRepl repl : replSet )
						if( repl.isRunning( queryId ) )
							repl.ticketFinished( queryId, ticketId, ticketFinishedMsg.getReportEntrySet() );				
				
			
			return;
		}
		
		if( msg instanceof TicketFailedMsg ) {
									
			ticketFailedMsg = ( TicketFailedMsg )msg;
			
			ticket = ticketFailedMsg.getTicket();
			ticketId = ticketFailedMsg.getTicketId();
			script = ticketFailedMsg.getScript();
			stdOut = ticketFailedMsg.getStdOut();
			stdErr = ticketFailedMsg.getStdErr();
			e = ticketFailedMsg.getException();
			querySet = ticketQueryMap.get( ticket );

			clearTicket( ticket );
			cacheMap.remove( ticketId );
			
			if( querySet != null ) {
			
				for( UUID queryId : querySet )
					for( BaseRepl repl : replSet )
						if( repl.isRunning( queryId ) )
							repl.queryFailed( queryId, ticketId, e, script, stdOut, stdErr );
				
				for( UUID queryId : querySet ) {
					clearQuery( queryId );
					failedQuerySet.add( queryId );
				}
			}
			
			
			return;
		}
		
		throw new RuntimeException( "Message type "+msg.getClass()+" not recognized." );
	}
	
	@Override
	public synchronized QualifiedTicket requestTicket( BaseRepl repl, UUID queryId, ApplyExpr applyExpr ) throws HasFailedException {
		
		CompoundExpr ce;
		int channel;
		ForeignLambdaExpr lambda;
		Ticket ticket;
		long ticketId;
		QualifiedTicket qt;
		Prototype prototype;
		
		if( queryId == null )
			throw new NullPointerException( "Query ID must not be null." );
		
		if( applyExpr == null )
			throw new NullPointerException( "Apply expression must not be null." );
		
		if( repl == null )
			throw new NullPointerException( "REPL actor must not be null." );
		
		if( failedQuerySet.contains( queryId ) )
			throw new HasFailedException( "Query "+queryId+" has already failed. Dropping request." );
		
		if( log.isDebugEnabled() )
			log.debug( "Requesting ticket for "+applyExpr.toString().replace( '\n', ' ') );
		
		ce = applyExpr.getTaskExpr();
		try {

			if( ce.getNumAtom() != 1 )
				throw new RuntimeException( "Excepted singular task expression." );
			
			if( !( ce.getSingleExpr( 0 ) instanceof ForeignLambdaExpr ) )
				throw new RuntimeException( "Expected foreign lambda expression." );

			lambda = ( ForeignLambdaExpr )ce.getSingleExpr( 0 ); 
		}
		catch( NotDerivableException e ) {
			throw new RuntimeException( "Cannot derive cardinality of task expression." );
		}
				
		prototype = lambda.getPrototype();
		
		for( NameExpr name : prototype.getParamNameSet() ) {
			
			if( name instanceof ReduceVar )
				continue;
			
			if( name.getId().equals( CfSemanticModelVisitor.LABEL_TASK ) )
				continue;
			
			try {
				
				if( applyExpr.getExpr( name ).getNumAtom() != 1 )
					throw new RuntimeException( "Expected singular application." );
			}
			catch( NotDerivableException e ) {
				throw new RuntimeException( "Cannot derive cardinality of parameter "+name );
			}
			catch( NotBoundException e ) {
				throw new RuntimeException( e.getMessage() );
			}
		}
		
		replSet.add( repl );
		
		channel = applyExpr.getChannel();
		
		
			
		ticket = new Ticket( lambda, applyExpr, runId );

		ticketId = ticket.getTicketId();
		
		if( cacheMap.containsKey( ticketId ) ) {
			
			if( log.isDebugEnabled() )
				log.debug( "Returning ticket "+ticketId+" from cache." );
			
			ticket = cacheMap.get( ticketId );
			putTicket( queryId, ticket );
			qt = new QualifiedTicket( ticket, channel );
		}
		else {
			
			if( log.isDebugEnabled() )
				log.debug( "Creating new ticket "+ticketId+"." );			
			
			qt = new QualifiedTicket( ticket, channel );
			putTicket( queryId, ticket );
			cacheMap.put( ticketId, ticket );
			cre.sendMsg( new TicketReadyMsg( this, queryId, ticket ) );
		}
		
		if( log.isDebugEnabled() )
			log.debug( "Substitute with ticket: "+ticket.toString().replace( '\n', ' ' ) );

		return qt;
	}

	@Override
	protected void shutdown() {
		// nothing to do
	}
	
	private void putTicket( UUID queryId, Ticket ticket ) {
		
		Set<Ticket> ticketSet;
		Set<UUID> uuidSet;
		
		if( queryId == null )
			throw new NullPointerException( "Run must not be null." );
		
		if( ticket == null )
			throw new NullPointerException( "Ticket must not be null." );
		
		ticketSet = queryTicketMap.get( queryId );
		if( ticketSet == null ) {
			ticketSet = new HashSet<>();
			queryTicketMap.put( queryId, ticketSet );
		}
		
		uuidSet = ticketQueryMap.get( ticket );
		if( uuidSet == null ) {
			uuidSet = new HashSet<>();
			ticketQueryMap.put( ticket, uuidSet );
		}
		
		if( !ticket.isNormal() )
			throw new RuntimeException( "Ticket not ready." );
		
		ticketSet.add( ticket );
		uuidSet.add( queryId );
		

	}
	
	private void clearTicket( Ticket ticket ) {
		
		ticketQueryMap.remove( ticket );
		for( Set<Ticket> ticketSet : queryTicketMap.values() )
			ticketSet.remove( ticket );
	}
	
	private void clearQuery( UUID queryId ) {
		
		queryTicketMap.remove( queryId );
		for( Set<UUID> querySet : ticketQueryMap.values() )
			querySet.remove( queryId );
	}

}
