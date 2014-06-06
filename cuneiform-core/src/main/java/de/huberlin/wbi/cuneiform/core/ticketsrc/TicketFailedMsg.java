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
			s += " Exception: \""+e.getClass().getName();
			if( e.getMessage() != null )
				s += " "+e.getMessage();
			s += "\"";
		}
		
		if( stdErr != null )
			s += " Error channel: \""+stdErr.replace( '\n', ' ' )+"\"";
		
		return "{ ticketFailed, "+ticket.getTicketId()+","+s+" }";
	}
}
