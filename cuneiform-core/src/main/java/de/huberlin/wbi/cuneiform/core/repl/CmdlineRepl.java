package de.huberlin.wbi.cuneiform.core.repl;

import java.util.UUID;

import org.apache.commons.logging.Log;

import de.huberlin.wbi.cuneiform.core.ticketsrc.ReplTicketSrc;

public class CmdlineRepl extends InteractiveRepl {

	public CmdlineRepl( ReplTicketSrc ticketSrc, Log statLog ) {
		super( ticketSrc, statLog );
	}
	
	@Override
	public synchronized void queryFailedPost( UUID queryId, Long ticketId, Exception e, String script, String stdOut, String stdErr ) {
		super.queryFailedPost( queryId, ticketId, e, script, stdOut, stdErr );
		System.exit( -1 );
	}

}
