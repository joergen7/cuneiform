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

package de.huberlin.wbi.cuneiform.core.cre;

import java.util.UUID;

import de.huberlin.wbi.cuneiform.core.actormodel.Actor;
import de.huberlin.wbi.cuneiform.core.actormodel.Message;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class TicketReadyMsg extends Message {

	private UUID runId;
	private Ticket ticket;

	public TicketReadyMsg( Actor sender, UUID runId, Ticket ticket ) {
		super( sender );
		setRunId( runId );
		setTicket( ticket );
	}
	
	public UUID getRunId() {
		return runId;
	}
	
	public Ticket getTicket() {
		return ticket;
	}

	public void setRunId( UUID runId ) {
		
		if( runId == null )
			throw new NullPointerException( "Run ID must not be null." );
		
		this.runId = runId;
	}
	
	public void setTicket( Ticket ticket ) {
		
		if( ticket == null )
			throw new NullPointerException( "Ticket set must not be null." );
				
		this.ticket = ticket;
		
	}
	
	@Override
	public String toString() {
		return "{ ticketReady, \""+runId+"\", "+ticket.getTicketId()+", \""+ticket.toString().replace( '\n', ' ' )+"\" }";
	}


}
