package de.huberlin.cuneiform.dax.repl;

import java.util.UUID;

import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;
import de.huberlin.wbi.cuneiform.core.semanticmodel.CompoundExpr;
import de.huberlin.wbi.cuneiform.core.ticketsrc.TicketSrcActor;

public class DaxRepl extends BaseRepl {

	public DaxRepl( TicketSrcActor ticketSrc ) {
		super( ticketSrc );
	}

	@Override
	public void queryFailedPost( UUID queryId, long ticketId, Exception e,
			String script, String stdOut, String stdErr ) {}

	@Override
	public void queryFinishedPost( UUID queryId, CompoundExpr result ) {}

	@Override
	public void queryStartedPost( UUID runId ) {}

}
