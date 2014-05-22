package de.huberlin.wbi.cuneiform.core.ticketsrc;

import de.huberlin.wbi.cuneiform.core.actormodel.Message;
import de.huberlin.wbi.cuneiform.core.cre.BaseCreActor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class TicketFailedMsg extends Message {

	private final String script;
	private final String stdErr;
	private final String stdOut;
	private final Ticket ticket;
	
	public TicketFailedMsg(
		BaseCreActor sender,
		Ticket ticket,
		String script,
		String stdOut,
		String stdErr ) {
		
		super( sender );
		
		if( script == null )
			throw new NullPointerException( "Script must not be null." );
		
		if( script.isEmpty() )
			throw new RuntimeException( "Script must not be empty." );
		
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
		this.script = script;
	}
	
	public String getStdErr() {
		return stdErr;
	}
	
	public String getStdOut() {
		return stdOut;
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
	
	public boolean hasStdErr() {
		return stdErr != null;
	}
	
	public boolean hasStdOut() {
		return stdOut != null;
	}
	
	@Override
	public String toString() {
		return "{ ticketFailed, \""+stdErr.replace( '\n', ' ' )+"\" }";
	}
}
