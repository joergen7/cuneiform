package de.huberlin.wbi.cuneiform.core.ticketsrc;

import java.util.Set;
import java.util.UUID;

import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public interface ReplTicketSrc extends NodeVisitorTicketSrc {

	public Set<Ticket> getTicketSet( UUID queryId );
}
