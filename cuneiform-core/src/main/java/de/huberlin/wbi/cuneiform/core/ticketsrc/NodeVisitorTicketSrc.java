package de.huberlin.wbi.cuneiform.core.ticketsrc;

import java.util.Set;
import java.util.UUID;

import de.huberlin.wbi.cuneiform.core.repl.BaseRepl;
import de.huberlin.wbi.cuneiform.core.semanticmodel.ApplyExpr;
import de.huberlin.wbi.cuneiform.core.semanticmodel.HasFailedException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.QualifiedTicket;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public interface NodeVisitorTicketSrc {

	public UUID getRunId();
	public boolean isQueueClear( UUID queryId )throws HasFailedException;
	public QualifiedTicket requestTicket( BaseRepl repl, UUID queryId, ApplyExpr applyExpr )throws HasFailedException;
	public Set<Ticket> getTicketSet( UUID queryId );

}
