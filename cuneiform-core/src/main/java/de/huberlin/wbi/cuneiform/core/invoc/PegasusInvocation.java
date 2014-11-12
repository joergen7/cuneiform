package de.huberlin.wbi.cuneiform.core.invoc;

import de.huberlin.wbi.cuneiform.core.semanticmodel.NotDerivableException;
import de.huberlin.wbi.cuneiform.core.semanticmodel.Ticket;

public class PegasusInvocation extends BashInvocation {

	public PegasusInvocation( Ticket ticket ) {
		super(ticket);
	}
	
	@Override
	public String getFunDef() throws NotDerivableException {
		return defFunctionLog()
			+defFunctionLogFile();
	}

	@Override
	public String getOutputRename() {
		return "";
	}
}
