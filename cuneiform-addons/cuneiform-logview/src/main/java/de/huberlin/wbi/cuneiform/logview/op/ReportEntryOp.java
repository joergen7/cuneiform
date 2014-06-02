package de.huberlin.wbi.cuneiform.logview.op;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;

public interface ReportEntryOp {
	public void process( JsonReportEntry entry );
	
}
