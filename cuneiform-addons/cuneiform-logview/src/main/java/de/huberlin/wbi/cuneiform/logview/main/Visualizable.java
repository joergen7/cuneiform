package de.huberlin.wbi.cuneiform.logview.main;

import javax.swing.JPanel;

import de.huberlin.wbi.cuneiform.core.semanticmodel.JsonReportEntry;

public abstract class Visualizable extends JPanel {

	private static final long serialVersionUID = -1940465202761413892L;

	public abstract void register( JsonReportEntry entry ) throws Exception;
	public abstract void clear();
	public abstract void updateView();
	
}
