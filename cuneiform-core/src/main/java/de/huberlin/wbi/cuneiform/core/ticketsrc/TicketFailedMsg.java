package de.huberlin.wbi.cuneiform.core.ticketsrc;

import de.huberlin.wbi.cuneiform.core.actormodel.Message;
import de.huberlin.wbi.cuneiform.core.cre.BaseCreActor;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class TicketFailedMsg extends Message {

	private String script;
	private String stdErr;
	private String stdOut;
	private Ticket ticket;
	
	public TicketFailedMsg(
		BaseCreActor sender,
		Ticket ticket,
		String script,
		String stdOut,
		String stdErr ) {
		
		super( sender );
		
		setTicket( ticket );
		setScript( script );
		setStdErr( stdErr );
		setStdOut( stdOut );
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
	
	public void setScript( String script ) {
		
		if( script == null )
			throw new NullPointerException( "Script must not be null." );
		
		if( script.isEmpty() )
			throw new RuntimeException( "Script must not be empty." );
		
		this.script = script;
	}
	
	public void setStdOut( String stdOut ) {
		
		if( stdOut == null ) {
			this.stdOut = null;
			return;
		}
		
		if( stdOut.isEmpty() ) {
			this.stdOut = null;
			return;
		}
		
		this.stdOut = stdOut;
	}
	
	public void setStdErr( String stdErr ) {
		
		if( stdErr == null ) {
			this.stdErr = null;
			return;
		}
		
		if( stdErr.isEmpty() ) {
			this.stdErr = null;
			return;
		}
		
		this.stdErr = stdErr;
	}
	
	public void setTicket( Ticket ticket ) {
		
		if( ticket == null )
			throw new NullPointerException( "Ticket must not be null." );
		
		this.ticket = ticket;
	}

	@Override
	public String toString() {
		return "{ ticketFailed, \""+stdErr.replace( '\n', ' ' )+"\" }";
	}
}
