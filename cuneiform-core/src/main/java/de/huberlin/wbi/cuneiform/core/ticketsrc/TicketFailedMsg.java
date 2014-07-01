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

import de.huberlin.wbi.cuneiform.core.actormodel.Message;
import de.huberlin.wbi.cuneiform.core.cre.BaseCreActor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class TicketFailedMsg extends Message {

	private final String script;
	private final String stdErr;
	private final String stdOut;
	private final Ticket ticket;
	private final Exception e;
	
	public TicketFailedMsg(
		BaseCreActor sender,
		Ticket ticket,
		Exception e,
		String script,
		String stdOut,
		String stdErr ) {
		
		super( sender );
		
		if( script == null )
			this.script = null;
		else
			if( script.isEmpty() )
				this.script = null;
			else
				this.script = script;
		
		if( ticket == null )
			throw new NullPointerException( "Ticket must not be null." );
		
		if( stdOut == null )
			this.stdOut = null;
		else		
			if( stdOut.isEmpty() )
				this.stdOut = null;
			else
				this.stdOut = stdOut;
		
		if( stdErr == null )
			this.stdErr = null;
		else
			if( stdErr.isEmpty() )
				this.stdErr = null;
			else
				this.stdErr = stdErr;
				
		this.ticket = ticket;
		this.e = e;
	}
	
	public String getStdErr() {
		return stdErr;
	}
	
	public String getStdOut() {
		return stdOut;
	}
	
	public Exception getException() {
		return e;
	}
	
	public String getScript() {
		return script;
	}
	
	public Ticket getTicket() {
		return ticket;
	}
	
	public long getTicketId() {		
		return ticket.getTicketId();
	}
	
	public boolean hasScript() {
		return script != null;
	}
	
	public boolean hasStdErr() {
		return stdErr != null;
	}
	
	public boolean hasStdOut() {
		return stdOut != null;
	}
	
	public boolean hasException() {
		return e != null;
	}
	
	@Override
	public String toString() {
		
		String s;

		s = "";
		
		if( e != null ) {
			s += " "+e.getClass().getName();
			
			if( e.getMessage() != null )
				s += ": \""+e.getMessage()+"\"";
		}
		
		if( stdOut != null )
			s += " Output channel: \""+stdOut.replace( '\n', ' ' )+"\"";

		if( stdErr != null )
			s += " Error channel: \""+stdErr.replace( '\n', ' ' )+"\"";
		
		
		return "{ ticketFailed, "+ticket.getTicketId()+","+s+" }";
	}
}
